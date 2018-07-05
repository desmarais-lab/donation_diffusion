library(tidyverse)
library(boxr)
library(stringr)

# Read 'data/EL_16.csv' from box
box_auth()
df = box_read_csv(file_id = '302149820582', fread = TRUE)
date_low <- as.Date('2015-01-01')
date_high <- as.Date('2017-01-01')

## This mirrors the first pre processing step in 2_make_netinf_dataset.R
b = df
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

# Load the donor information file to get all info required for catalist matching
# and join to donation count df

## Read 'VLC_16.csv' from box
donor_info = box_read_csv(file_id = '302126365533', fread = TRUE)

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
box_write(df, filename = 'stratified_donor_sample.csv', write_fun = write_csv, 
          dir_id = '50855821402')
