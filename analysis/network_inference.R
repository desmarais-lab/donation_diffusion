library(tidyverse)
devtools::install('~/projects/NetworkInference/')
library(NetworkInference)
library(microbenchmark)

df <- read_csv('../data/EL_14.csv') %>%
    filter(!is.element(tran_tp, 
                       c('19', '24A', '24C', '24E', '24F', '24N', '29')),
           amt > 0, 
           !is.na(amt), !is.na(date), !is.na(donor_tp), !is.na(recip_tp)) %>%
    mutate(date = as.Date(date, '%m/%d/%Y')) %>%
    filter(date > as.Date('2013-01-01')) %>%
    group_by(donor, recip) %>%
    arrange(date) %>%
    summarize(amt = amt[1], date = date[1], donor_tp = donor_tp[1],
              recip_tp = recip_tp[1]) 

# Subset the data. Remove:
# - Recipients with less than 2 unique donors
# - Donors that give to only one candidate
recip_smry <- group_by(df, recip) %>%
    summarize(n_donors = length(unique(donor))) %>%
    arrange(desc(n_donors)) %>% 
    head(1000)

donor_smry <- group_by(df, donor) %>%
    summarize(n_recips = length(unique(recip))) %>%
    arrange(desc(n_recips)) %>%
    head(4000)

dat <- left_join(df, recip_smry, by=c('recip')) %>%
    left_join(donor_smry, by=c('donor')) %>%
    filter(!is.na(n_donors), !is.na(n_recips), n_donors > 1, n_recips > 1)

a = group_by(dat, recip) %>%
    summarize(n_donors = length(unique(donor)))
b = group_by(dat, donor) %>%
    summarize(n_recips = length(unique(recip)))


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
