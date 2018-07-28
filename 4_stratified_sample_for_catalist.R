library(tidyverse)
library(boxr)
library(stringr)
library(yaml)

config = yaml.load_file('0_config.yml')
LOCAL_DATA = config$LOCAL_DATA

# Here we read the EL_16_old.csv and VLC_16_old.csv files that where used 
# when the sample was created.
# Since then OS released additional records which we included in all other 
# analyses. Hence, the population here does not match the actual population 
# of records available now.
if(!is.null(LOCAL_DATA)) {
    df = read_csv(paste0(LOCAL_DATA, 'EL_16_old.csv'))
    donor_info = read_csv(paste0(LOCAL_DATA, 'VLC_16_old.csv'))
} else {
    box_auth()
    # Read 'data/EL_16.csv' from box
    df = box_read_csv(file_id = '307970249398')
    donor_info = box_read_csv(file_id = '307973560952', fread = TRUE)
}

date_low <- as.Date('2015-01-01')
date_high <- as.Date('2017-01-01')

## This mirrors the first pre processing step in 2_make_netinf_dataset.R
df <- df %>%
  select(Donor_ID, Recip_ID, Amt, Tran_Tp, Recip_Tp, Date, Donor_Tp) %>%
  na.omit() %>%
  mutate(Date = as.Date(Date, '%m/%d/%Y')) %>%
  filter(!is.element(Tran_Tp, c('19', '24A', '24C', '24E', '24F', 
                                '24N', '29')),
         Amt > 0, Date >= date_low, Date < date_high, Recip_Tp == 'CAND') %>%
  group_by(Donor_ID, Recip_ID) %>%
  arrange(Date) %>%
  filter(row_number() == 1)

donors <- group_by(df, Donor_ID) %>%
  summarize(Donor_Tp = Donor_Tp[1],
            n_records = n())

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Take a stratified sample
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

population <- inner_join(donors, donor_info, by = c("Donor_ID" = "Actor_ID")) %>%
  filter(!is.na(Name))

set.seed(678140)
samp1 <- filter(population, n_records == 1) %>%
  mutate(s = sample(1:nrow(.), nrow(.), replace=FALSE)) %>%
  filter(s <= 1000) %>%
  mutate(stratum = "1")
samp2_4 <- filter(population, n_records > 1, n_records < 5) %>%
  mutate(s = sample(1:nrow(.), nrow(.), replace=FALSE)) %>%
  filter(s <= 1000) %>%
  mutate(stratum = "2_4")

out <- rbind(samp1, samp2_4) %>%
  select(Name, n_records, stratum, Donor_ID)

#split names and zips for Catalist match
last<-gsub( ",.*$", "", out$Donor_ID)
first<- str_match(out$Donor_ID, ", (.*?)_")[,2]
first<-gsub(" MS","",first) #remove titles
first<-gsub(" MRS","",first)
first<-gsub(" MR","",first)
first<-gsub(" DR","",first)
zip<-sub('.*_', '', out$Donor_ID)

out$First<-first
out$Last<-last
out$Zip<-zip

## Write files to box in dir 'Strategic_Donors/final_paper_data/'
if(!is.null(LOCAL_DATA)) {
    write_csv(out, paste0(LOCAL_DATA, 'stratified_donor_sample.csv'))
} else {
    box_write(out, filename = 'stratified_donor_sample.csv', write_fun = write_csv, 
          dir_id = '50855821402')
}


# This doesn't replicate the sample we sent to catalyst