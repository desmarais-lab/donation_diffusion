#devtools::install_github('desmarais-lab/NetworkInference', ref = 'devel')
devtools::load_all('../../NetworkInference')
library(tidyverse)
#library(NetworkInference)

# Read and basic preproc
year <- as.integer(commandArgs(trailingOnly = TRUE)[1])
N <- as.integer(commandArgs(trailingOnly = TRUE)[2])

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
    arrange(Date) %>%
    # Only keep the first donation (in time) for each donor - recipient dyad
    filter(row_number() == 1)
nrow_with_isolates <- nrow(df)

# Subset the data. Remove:
# - Recipients with less than 2 unique donors
# - Donors that give to only one candidate
# - Iteratively remove them untill all isolates are gone:
remove_isolates <- function(df, isolate_threshold) {
    n_row_diff <- 1
    while(n_row_diff > 0) {
        n_row_before <- nrow(df)
        recip_smry <- group_by(df, Recip_ID) %>%
            summarize(n_donors = length(unique(Donor_ID)))
        donor_smry <- group_by(df, Donor_ID) %>%
            summarize(n_recips = length(unique(Recip_ID)))
        df <- left_join(df, recip_smry, by=c('Recip_ID')) %>%
            left_join(donor_smry, by=c('Donor_ID')) %>%
            filter(n_donors > isolate_threshold, 
                   n_recips > isolate_threshold) %>%
            select(-n_donors, -n_recips)
        n_row_diff <- n_row_before - nrow(df)
        cat(paste0('Removed ', n_row_diff, ' rows.\n'))
    }
    return(df)
}

# First remove donors and candidates that give/receive to/from only 1
cat(paste(length(unique(df$Donor_ID)), 'unique donors, removing isolates...\n'))
isolate_threshold <- 1
df <- remove_isolates(df, isolate_threshold)

# If there are more than N donors, increase threshold
while(length(unique(df$Donor_ID)) > N){
    isolate_threshold <- isolate_threshold + 1
    l <- length(unique(df$Donor_ID))
    cat(paste(l, 'unique Donors increasing isolate threshold to', 
              isolate_threshold, '\n'))
    df <- remove_isolates(df, isolate_threshold)
}
nrow_without_isolates <- nrow(df)

#donor_smry <- group_by(df, Donor_ID) %>%
#    summarize(n_recips = length(unique(Recip_ID))) %>%
#    arrange(desc(n_recips)) %>%
#    mutate(in_top = ifelse(row_number() <= N, TRUE, FALSE))
# 
#df <-left_join(df, donor_smry, by=c('Donor_ID')) %>%
#    filter(in_top) %>%
#    select(-n_recips, -in_top)
nrow_most_active <- nrow(df)



df$integer_date <- as.integer(df$Date)

## Descriptives
n_donors <- length(unique(df$Donor_ID))
n_recips <- length(unique(df$Recip_ID))
cat(paste0('Number of donors: ', n_donors, '\n'))
cat(paste0('Number of recipients: ', n_recips, '\n'))
filter_info <- c(nrow_initial, nrow_with_isolates, nrow_without_isolates, 
                 nrow_most_active)

# Fit the network
cascades <- as_cascade_long(df, cascade_node_name = 'Donor_ID', 
                            event_time = 'integer_date', 
                            cascade_id = 'Recip_ID',
                            node_names = unique(df$Donor_ID))
res <- netinf(cascades, n_edges = 15000, lambda = 0.25)

output <- list('network' = res$net, 'trees' = res$trees, 'data' = df, 
               'filter_info' = filter_info)

save(output, file = paste0('../data/results/', year, '_donors_', N, '_output.RData'))
