library(tidyverse)

year <- 2016
setwd("~/Box Sync/Strategic_Donors")
infile <- paste0('./data/EL_', substr(as.character(year), 3, 4), '.csv')
date_low <- as.Date(paste0(as.character(year - 1), '-01-01'))
date_high <- as.Date(paste0(as.character(year + 1), '-01-01'))
## This mirrors the first pre processing step in network_inference.R
df <- read_csv(infile) %>%
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
donor_file <- './data/Indivs_16.csv'
donor_info <- read_csv(donor_file)
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


library(stringr)
#split names and zips for Catalist match
last<-gsub( ",.*$", "", out$Donor_ID)
first<- str_match(out$Donor_ID, ", (.*?)_")[,2]
first<-gsub(" MS","",first) #remove titles
first<-gsub(" MRS","",first)
first<-gsub(" MR","",first)
first<-gsub(" DR","",first)
zip<-sub('.*_', '', out$Donor_ID)
  
#add
out$First<-first
out$Last<-last
out$Zip<-zip

write_csv(out, path = './data/stratified_donor_sample.csv')
