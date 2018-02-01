library(tidyverse)

source('remove_isolates.R')

year <- 2016
# How many donors should be included
n_nodes <- 100
infile <- paste0('../data/EL_', substr(as.character(year), 3, 4), '.csv')
date_low <- as.Date(paste0(as.character(year - 1), '-01-01'))
date_high <- as.Date(paste0(as.character(year + 1), '-01-01'))

df <- read_csv(infile)
nrow_initial <- nrow(df)

df <- df %>%
    select(Donor_ID, Recip_ID, Amt, Tran_Tp, Recip_Tp, Date, Donor_Tp) %>%
    # Remove rows with missing data
    na.omit() %>%
    mutate(Date = as.Date(Date, '%m/%d/%Y')) %>%
    # Remove transactions that are not considered donations
    # Remove transactions with negative and 0 amount (refunds)
    # Rows with transaction dates outside the period
    # With non-candidate recipients
    filter(!is.element(Tran_Tp, c('19', '24A', '24C', '24E', '24F', 
                                  '24N', '29')),
           Amt > 0, Date >= date_low, Date < date_high, Recip_Tp == 'CAND') %>%
    group_by(Donor_ID, Recip_ID) %>%
    # Only keep the first donation (in time) for each donor - recipient dyad
    arrange(Date) %>%
    filter(row_number() == 1)

# Subset the data. Remove:
# - Recipients with less than 2 unique donors
# - Donors that give to only one candidate
# - Iteratively remove them untill all isolates are gone:
isolate_threshold <- 1
df <- remove_isolates(df, isolate_threshold)
df$integer_date <- as.integer(df$Date)
write_csv(df, '../data/data_for_netinf.R')
