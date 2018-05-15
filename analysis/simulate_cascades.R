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

# For each candidate 1000 cascades from each model under all three conditions





