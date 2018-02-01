devtools::install_github('desmarais-lab/NetworkInference', ref = 'devel')

library(tidyverse)
library(NetworkInference)
source('../data_processing/remove_isolates.R')

# Number of nodes to use for inference
n_nodes <- 5000

# Read the preprocessed data (see `make_netinf_data.R` for details)
df <- read_csv('../data/data_for_netinf.R')

# If there are more than N donors, increase threshold
isolate_threshold <- 2
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
filter_info <- c(nrow_initial, nrow_with_isolates, nrow_without_isolates, 
                 nrow_most_active)

# Fit the network
cascades <- as_cascade_long(df, cascade_node_name = 'Donor_ID', 
                            event_time = 'integer_date', 
                            cascade_id = 'Recip_ID',
                            node_names = unique(df$Donor_ID))
res <- netinf(cascades, n_edges = 10000, lambda = 0.25)

output <- list('network' = res$net, 'trees' = res$trees, 'data' = df, 
               'filter_info' = filter_info)

save(output, file = paste0('../data/results/', year, '_donors_', N, '_output.RData'))
