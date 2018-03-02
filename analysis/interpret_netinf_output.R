library(tidyverse)
library(NetworkInference)
library(ggplot2)
source('../data_processing/remove_isolates.R')
source('plot_theme.R')

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Config
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
DONDAT = '../data/data_for_netinf.R' # should be .csv change at some point
RESDIR = '../data/results/'

get_network = function(threshold) {
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
    #network = filter(network, p_value <= 0.05)
    network$threshold = threshold
    return(network)
}

get_n_donors = function(threshold, data) {
    dat = remove_isolates(data, threshold)
    donors = group_by(dat, Donor_ID) %>%
    summarize(total_amount = sum(Amt),
              n_donations = n())  
    return(nrow(donors))
}

# Load the donation data
df = read_csv(DONDAT)

# Load latest version of all networks 
thresholds = c(5, seq(6, 18, 2))
networks = lapply(thresholds, get_network)
networks = tbl_df(do.call(rbind, networks))

# Get the number of unique donors for each base dataset
n_donors_in_data = sapply(thresholds, get_n_donors, data = df)
n_donors_in_data = data_frame(threshold = thresholds, n_donors = n_donors)


pdat = left_join(networks, n_donors, by = "threshold") %>%
    group_by(threshold) %>%
    summarize(n_donors_in_network_origin = length(unique(origin_node)),
              n_donors_in_network_destination = length(unique(destination_node)),
              perc_in_data_origin = n_donors_in_network_origin / n_donors[1],
              n_donors_in_data = n_donors[1],
              n_edges = n())
pdat