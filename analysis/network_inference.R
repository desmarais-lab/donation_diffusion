library(tidyverse)
devtools::install('~/projects/NetworkInference/')
library(NetworkInference)
library(microbenchmark)

# Read and basic preproc
df <- read_csv('../data/EL_14.csv') %>%
    # Remove transactions that are not considered donations
    filter(!is.element(Tran_Tp, 
                       c('19', '24A', '24C', '24E', '24F', '24N', '29')),
    # Remove transactions with negative and 0 amount (refunds)
           Amt > 0, 
    # Remove transactions with missing amount
           !is.na(Amt)) %>%
    # Some dates are not in range of data prob data mistakes
    mutate(Date = as.Date(Date, '%m/%d/%Y')) %>%
    filter(Date >= as.Date('2013-01-01'), Date < as.Date("2015-01-01")) %>%
    group_by(Donor_ID, Recip_ID) %>%
    arrange(Date) %>%
    # Only keep the first donation (in time) for each donor - recipient dyad
    filter(row_number() == 1)
        
    
# Subset the data. Remove:
# - Recipients with less than 2 unique donors
# - Donors that give to only one candidate
# - Iteratively remove them untill all isolates are gone:
n_row_diff <- 1
while(n_row_diff > 0) {
        
    n_row_before <- nrow(df)
    recip_smry <- group_by(df, Recip_ID) %>%
        summarize(n_donors = length(unique(Donor_ID)))
    
    donor_smry <- group_by(df, Donor_ID) %>%
        summarize(n_recips = length(unique(Recip_ID)))
    
    df <- left_join(df, recip_smry, by=c('Recip_ID')) %>%
        left_join(donor_smry, by=c('Donor_ID')) %>%
        filter(n_donors > 1, n_recips > 1) %>%
        select(-n_donors, -n_recips)
    
    n_row_diff <- n_row_before - nrow(df)
    cat(paste0('Removed ', n_row_diff, ' rows.\n'))
}
df$integer_date <- as.integer(df$Date)

# Final transformation: Only use the N most active donors
N <- 5000
donor_smry <- group_by(df, Donor_ID) %>%
    summarize(n_recips = length(unique(Recip_ID))) %>%
    arrange(desc(n_recips)) %>%
    mutate(in_top = ifelse(row_number() <= N, TRUE, FALSE))
 
df <-left_join(df, donor_smry, by=c('Donor_ID')) %>%
    filter(in_top) %>%
    select(-n_recips, -in_top)

## Descriptives
length(unique(df$Donor_ID))
length(unique(df$Recip_ID))

recip_smry <- group_by(df, Recip_ID) %>%
    summarize(n_donors = length(unique(Donor_ID)),
              mean_donation = mean(Amt),
              median_donation = median(Amt))

donor_smry <- group_by(df, Donor_ID) %>%
    summarize(n_recips = length(unique(Recip_ID)),
              mean_donation = mean(Amt),
              median_donation = median(Amt))

# Plot some descriptives
cascades <- as_cascade_long(df, cascade_node_name = 'Donor_ID', 
                            event_time = 'integer_date', 
                            cascade_id = 'Recip_ID',
                            node_names = unique(df$Donor_ID))

smry <- summary(cascades) 
res <- netinf(cascades, n_edges = 10000, lambda = 5)
save(res, 'diffnet.RData')
