# Prep all data ready to be loaded for a single candidate cascade job

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

# Load the individual donors file
actors_file = '255302928759'
actors_file_name = 'VLC_16_full.csv'
actors = box_read_csv(actors_file) %>% tbl_df()

## Remove non individual donors and donors without covariates
ind_actors = filter(actors, Ent_Typ == 'IND', !is.na(ideology),
                    !is.na(ind_cd), !is.na(ind_state))
ind_actor_ids = ind_actors$Actor_ID

# Load the inferred diffusion network
netinf_file = '280757684849'
netinf_file_name = 'netinf_threshold_8_iter_3.RData'
box_dl(netinf_file)
load(netinf_file_name)
unlink(netinf_file_name)

## 0.025 p-value threshold
netinf_network = out$netinf_out[1:min(which(out$netinf_out$p_value > 0.025)), ] %>%
    filter(origin_node %in% ind_actor_ids, destination_node %in% ind_actor_ids)

# Load the donation data
donation_file = '292888533329'
donation_file_name = 'data_for_netinf.csv'
donation_data = box_read_csv(donation_file) %>% tbl_df()

# Get the donation data as used for netinf
source('../data_processing/remove_isolates.R')
donation_data = remove_isolates(donation_data, 8)
donation_cascades = as_cascade_long(donation_data, 
                                    cascade_node_name = 'Donor_ID',
                                    cascade_id = 'Recip_ID', 
                                    event_time = 'integer_date')


# Transform the ergm networks to edgelists
to_edgelist = function(network) {
    el = as.edgelist(network)
    vnames = attr(el, 'vnames')
    out = as.data.frame(el) %>% 
        mutate(origin_node = vnames[el[, 1]],
               destination_node = vnames[el[, 2]]) %>%
        select(-V1, -V2) %>%
        tbl_df()
    class(out) = c('diffnet', 'data.frame')
    return(out)
}
directional_networks = lapply(directional.networks, to_edgelist)
spatial_networks = lapply(spatial.networks, to_edgelist)

global_censoring_time = max(donation_data$integer_date)

models = list('directional_networks' = directional_networks, 
              'spatial_networks' = spatial_networks, 
              'netinf_network' = netinf_network)
save(models, donation_cascades, global_censoring_time, 
     file = '../data/casc_sim_data.RData')