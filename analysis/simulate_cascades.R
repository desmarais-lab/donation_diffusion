#devtools::install_github('desmarais-lab/NetworkInference')
library(NetworkInference)
library(boxr)
library(tidyverse)
library(ergm)

box_auth()
network_simulation_folder = '49466002727'

# Load the simulated ergm networks
ergm_sim_file = '292848476812'
ergm_sim_file_name = 'ergm_simulation_results.RData'
box_dl(ergm_sim_file)    
load(ergm_sim_file_name)
unlink(ergm_sim_file_name)

# Load the inferred diffusion network
netinf_file = '280757684849'
netinf_file_name = 'netinf_threshold_8_iter_3.RData'
box_dl(netinf_file)
load(netinf_file_name)
unlink(netinf_file_name)
netinf.network = select(out$netinf_out, origin_node, destination_node)

# Load the donation data
donation_file = '292888533329'
donation_file_name = 'data_for_netinf.csv'
donation_data = box_read_csv(donation_file) %>% tbl_df()

# Functions
to_edgelist = function(network) {
    el = as.edgelist(network)
    vnames = attr(el, 'vnames')
    out = as.data.frame(el) %>% 
        mutate(origin_node = vnames[el[, 1]],
               destination_node = vnames[el[, 2]]) %>%
        select(-V1, -V2) %>%
        tbl_df()
    return(out)
}


# Get the donation data as used for netinf
source('../data_processing/remove_isolates.R')
donation_data = remove_isolates(donation_data, 8)
full_cascades = as_cascade_long(donation_data, cascade_node_name = 'Donor_ID',
                                cascade_id = 'Recip_ID', 
                                event_time = 'integer_date')

# For each candidate 1000 cascades from each model under all three conditions
candidates = unique(donation_data$Recip_ID)
diffmod_params = attr(netinf.network, 'diffusion_model_parameters')
diffmod = attr(netinf.network, 'diffusion_model')
general_cutoff_time = max(donation_data$integer_date)

# This function simulates n_sim cascades for each candidate based on the 
# provided diffusion_network
sim_cascade = function(diffusion_network, prop_observed, 
                       cascades, params, diffmod, max_time, n_sim) {
   
    n_observed = sapply(cascades$cascade_times, function(x) {
        round(length(x) * prop_observed, 0)  
    })
    
    partial_cascades = subset_cascade_n(cascades, n_observed)
    casc_ids = names(partial_cascades$cascade_nodes)
    
    sim_out = lapply(casc_ids, function(x) {
        s = Sys.time()
        pc = subset_cascade(partial_cascades, x)
        out = simulate_cascades(netinf.network, nsim = 1000, 
                                partial_cascade = pc, 
                                params = params, model = diffmod,
                                max_time = max_time)
        out$cascade_id = x
        print(Sys.time() - s)
        return(out)
    }) 
     
}

# Timings:
# for 10: ~42s
# for 1000: 

# In chunks of 10 cascades per candidate with 42 seconds per candidate:
# 808 candidates * 100 iterations of 10 * 3 partial cascade conditions * 3 models * 42 seconds:
# Batches of 10: 808 * 100 * 3 * 3 * 42 = 30542400 seconds = 353 days
# Batches of 1000: 808 * 3 * 3 * 3600 = 26179200 seconds = 303 days

    
subset_cascade_n <- function(cascade, ns) {
    casc_length <- length(cascade$cascade_nodes)    
    
    subset_times <- lapply(1:casc_length, function(i) {
        return(cascade$cascade_times[[i]][1:ns[i]])
    }) 
    subset_nodes <- lapply(1:casc_length, function(i) {
        return(cascade$cascade_nodes[[i]][1:ns[i]])
    }) 
    names(subset_times) <- names(subset_nodes) <- names(cascade$cascade_times)
    
    subset_node_names <- cascade$node_names
    out <- list(cascade_times = subset_times, cascade_nodes = subset_nodes,
                node_names = subset_node_names)
    class(out) <- c("cascade", "list")
    return(out)
}
