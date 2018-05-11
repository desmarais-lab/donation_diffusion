devtools::install_github('desmarais-lab/NetworkInference')

library(tidyverse)
library(NetworkInference)
library(microbenchmark)
source('../data_processing/remove_isolates.R')

args = commandArgs(trailingOnly=TRUE)
isolate_threshold = as.integer(args[1])
init_params = 0.012

# Read the preprocessed data (see `make_netinf_data.R` for details)
cat('threshold: ', isolate_threshold, '\n')
df <- read_csv('../data/data_for_netinf.R')
df <- remove_isolates(df, isolate_threshold)

cat('Number of donors', length(unique(df$Donor_ID)), '\n')

# If there are more than N donors, increase threshold
isolate_threshold <- 80
while(length(unique(df$Donor_ID)) > n_nodes) {
    isolate_threshold <- isolate_threshold + 1
    l <- length(unique(df$Donor_ID))
    cat(paste(l, 'unique Donors increasing isolate threshold to', 
              isolate_threshold, '\n'))
    df <- remove_isolates(df, isolate_threshold)
}

## Descriptives
n_donors <- length(unique(df$Donor_ID))
n_recips <- length(unique(df$Recip_ID))
cat(paste0('Number of donors: ', n_donors, '\n'))
cat(paste0('Number of recipients: ', n_recips, '\n'))
cat(paste0('Number of donations: ', nrow(df), '\n'))

# Fit the network
cascades <- as_cascade_long(df, cascade_node_name = 'Donor_ID', 
                            event_time = 'integer_date', 
                            cascade_id = 'Recip_ID')

res = netinf(cascades, params = init_params, max_iter = 1, n_edges = 10000)

# Estimate lambda from the diffusion times in the estimated trees
output <- list('network' = res$net, 'data' = df)
save(output, file = paste0('../data/results/', n_nodes, 'donors.RData'))
