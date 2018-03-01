library(tidyverse)
library(ggplot)
source('../data_processing/remove_isolates.R')
source('plot_theme.R')

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Config
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
DONDAT = '../data/data_for_netinf.R' # should be .csv change at some point
RESDIR = '../data/results/'

threshold = 19

# Load the donation data
df = read_csv(DONDAT)

# Get the same dataset that was used to infer the network
dat = remove_isolates(df, threshold)

# Load the inferred network (latest available iteration)
fl = list.files(RESDIR)
rel_networks = fl[grepl(paste0('_', threshold, "_"), fl)]
last_network = rel_networks[order(
    as.integer(
        gsub('.RData', '', sapply(
            strsplit(rel_networks, '_'), function(x) x[5])
            )
        ), decreasing = TRUE
    )[1]]
load(paste0(RESDIR, last_network))
network = out$netinf_out 

# Check for isolates
donors_in_network = unique(c(network$origin_node, network$destination_node))
din_dat = data_frame(Donor_ID = donors_in_network) %>%
    mutate(matched = 1)

donors = group_by(dat, Donor_ID) %>%
    summarize(total_amount = sum(Amt),
              n_donations = n()) %>%
    left_join(din_dat, by = "Donor_ID") %>%
    mutate(matched = !is.na(matched))


ggplot(donors) + 
    geom_boxplot(aes(x = matched, y = n_donations)) +
    scale_y_log10() +
    plot_theme
ggsave('~/Dropbox/Public/ind_isolate_distribution.png')

group_by(donors, matched) %>%
    summarize(n_donors = n(), 
              min_donations = min(n_donations),
              max_doations = max(n_donations),
              mean_donations = mean(n_donations),
              median_donations = median(n_donations), 
              mean_amount = mean(total_amount),
              median_amount = median(total_amount))
