library(tidyverse)
library(NetworkInference)
library(boxr)
source('../data_processing/remove_isolates.R')

isolate_threshold = 8
init_params = 0.012

# Read 'data/data_for_netinf.RData' from box see `make_netinf_data.R` for details
df <- box_read_csv(file_id = '292888533329')
df <- remove_isolates(df, isolate_threshold)

cat('Number of donors', length(unique(df$Donor_ID)), '\n')

# Fit the network
cascades <- as_cascade_long(df, cascade_node_name = 'Donor_ID', 
                            event_time = 'integer_date', 
                            cascade_id = 'Recip_ID')

iter = 1
while(TRUE) {
    res = netinf(cascades, trans_mod = 'exponential', params = init_params, 
                 p_value_cutoff = 0.05)
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
