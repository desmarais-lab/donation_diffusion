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

# Fit the network
cascades <- as_cascade_long(df, cascade_node_name = 'Donor_ID', 
                            event_time = 'integer_date', 
                            cascade_id = 'Recip_ID')

iter = 1
while(TRUE) {
    res = netinf(cascades, params = init_params, max_iter = 1, n_edges = 0.1)
    converged = attr(res, 'converged')
    out = list('netinf_out' = res, 'iteration' = iter,
               'converged' = converged)
    save(out, file = paste0('../data/results/netinf_threshold_', 
                            isolate_threshold, '_iter_', iter, '.RData'))   
    if(converged) break
    else init_params = attr(res, "diffusion_model_parameters")
    iter = iter + 1
    if(iter > 10) {
        print('Stopping without convergence')
        break
    }
}
