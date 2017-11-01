library(tidyverse)
devtools::install('~/projects/NetworkInference/')
library(NetworkInference)
library(microbenchmark)

df <- read_csv('../data/EL_14.csv') %>%
    filter(!is.element(tran_tp, 
                       c('19', '24A', '24C', '24E', '24F', '24N', '29')),
           amt > 0, 
           !is.na(amt)) %>%
    mutate(date = as.Date(date, '%m/%d/%Y'))
    
# Subset the data. Remove:
# - Recipients with less than 2 unique donors
# - Donors that give to only one candidate
recip_smry <- group_by(df, recip) %>%
    summarize(n_donors = length(unique(donor)))

donor_smry <- group_by(df, donor) %>%
    summarize(n_recips = length(unique(recip)))

dat <- left_join(df, recip_smry, by=c('recip')) %>%
    left_join(donor_smry, by=c('donor')) %>%
    filter(n_donors > 1, n_recips > 1)


# Donor types
length(unique(dat$donor))
length(unique(dat$recip))
    
cascades <- as_cascade_long(dat, cascade_node_name = 'donor', 
                            event_time = 'date', 
                            cascade_id = 'recip',
                            node_names = unique(dat$donor))

smry <- summary(cascades) 
res <- netinf(cascades, n_edges = 100, lambda = 1)
save(res, 'diffnet.RData')
print(time$time / 1e9)
