# Prep all data ready to be loaded for a single candidate cascade job

library(NetworkInference)
library(boxr)
library(tidyverse)
library(ergm)
library(yaml)

config = yaml.load_file('0_config.yml')
LOCAL_DATA = config$LOCAL_DATA
P_VALUE = config$P_VALUE
THRESHOLD = config$ISOLATE_THRESHOLD

if(!is.null(LOCAL_DATA)) {
    load(paste0(LOCAL_DATA, 'ergm_simulation_results_', THRESHOLD, '_pval_',
              P_VALUE, '.RData'))     
    ergm_sim_out = out
    actors = read_csv(paste0(LOCAL_DATA, 'vlc_16_full.csv'))
    load(paste0(LOCAL_DATA, 'netinf_network_threshold_', THRESHOLD, '.RData'))
    netinf_network = network
    donation_data = read_csv(paste0(LOCAL_DATA, 
                             'data_for_netinf_threshold_', THRESHOLD, '.csv'))
} else {
    box_auth()
    if(P_VALUE == 0.025) box_load(file_id = '308416233264')
    #else box_load(file_id = '302644314208')
    ergm_sim_out = list('spatial.networks' = spatial.networks,
                        'directional.networks' = directional.networks) 
    actors = box_read_csv(file_id = '308095557675')
    # Load netinf_network_threshold_THRESHOLD.RData
    box_load(file_id = '307983798270') 
    netinf_network = network
    donation_data = box_read_csv(
        file_id = config$data_for_netinf_threshold_8.csv)
}

## Remove non individual donors and donors without covariates
ind_actors = filter(actors, Ent_Typ == 'IND', !is.na(ideology),
                    !is.na(ind_cd), !is.na(ind_state))
ind_actor_ids = ind_actors$Actor_ID

## 0.025 p-value threshold
netinf_network = netinf_network[1:min(which(netinf_network$p_value > P_VALUE)), ] %>%
    filter(origin_node %in% ind_actor_ids, destination_node %in% ind_actor_ids)

# Get the donation data as used for netinf
donation_data = filter(donation_data, Donor_ID %in% ind_actor_ids)
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
directional_networks = lapply(ergm_sim_out$directional.networks, to_edgelist)
spatial_networks = lapply(ergm_sim_out$spatial.networks, to_edgelist)

global_censoring_time = max(donation_data$integer_date)

models = list('directional_networks' = directional_networks, 
              'spatial_networks' = spatial_networks, 
              'netinf_network' = netinf_network)
casc_sim_data = list(models = models, 
                     donation_cascades = donation_cascades, 
                     global_censoring_time = global_censoring_time)
if(!is.null(LOCAL_DATA)) {
    save(casc_sim_data, file = paste0(LOCAL_DATA, 'casc_sim_data.RData'))
} else {
    box_save(casc_sim_data, dir_id = '50855821402') 
}
