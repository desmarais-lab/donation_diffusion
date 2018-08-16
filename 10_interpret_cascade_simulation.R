library(tidyverse)
library(sna)
library(igraph)
library(NetworkInference)
library(boxr)
library(yaml)


config = yaml.load_file('0_config.yml')
LOCAL_DATA = config$LOCAL_DATA

#devtools::install_github('flinder/flindR')
pe = flindR::plot_elements()

SIM_RES_DIR = 'data/cascade_simulation_results/'

## Load all simulation results

#outfiles = list.files(SIM_RES_DIR, patter = '*.RData')
#i = 1
#for(f in outfiles) {
#    cat(i, f, '\n')
#    if(f == 'compiled_results.RData') next
#    load(paste0('data/cascade_simulation_results/', f))
#    if(i == 1) out = do.call(rbind, results)
#    else out = rbind(out, do.call(rbind, results))
#    i = i + 1 
#}
#
#simulation_results = tbl_df(out)
#save(simulation_results, 
#     file = 'data/cascade_simulation_results/compiled_results.RData')
#
#stop()
load('data/cascade_simulation_results/compiled_results.RData')
cutoff_time = 17166


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Some descriptives on the simulation
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
load('data/casc_sim_data.RData')

# Before restriction
donation_cascades = casc_sim_data$donation_cascades

## Number of candidates completely dropped
candidates = names(donation_cascades$cascade_nodes)
sim_candidates = unique(simulation_results$candidate)
dropped_candidates = candidates[which(!(candidates %in% sim_candidates))]
length(dropped_candidates)

## Number of successfull simulations (a simulation is not successfull if the 
## all seed nodes are isolates)
n_sims = group_by(simulation_results, network_type, proportion_observed, 
                   candidate) %>%
    summarize(count = length(unique(cascade_id))) 
ggplot(n_sims, aes(x = count)) +
    geom_histogram(color = 'white') +
    facet_wrap(~network_type, scales = "free")

n_sims %>%
    group_by(network_type) %>%
    summarize(average_n_simulations = mean(count),
              median_n_simulations = median(count))

## Average cascade length
casc_lengths = group_by(simulation_results, network_type, proportion_observed, 
                        cascade_id, candidate) %>% 
    summarize(n = n()) %>%
    group_by(network_type) %>%
    summarize(average_cascade_length = mean(n),
              median_cascade_length = median(n))

# After restriction

## Get total number of observed donations in the data
n_donations = sum(sapply(donation_cascades$cascade_nodes, length))

## For each simulated cascade cut off all donations made after n_donations is 
## reached
simulation_cut = group_by(simulation_results, network_type, proportion_observed,
                          cascade_id) %>% 
    arrange(network_type, proportion_observed, cascade_id, event_time) %>%
    mutate(rank = row_number()) %>%
    filter(rank < n_donations)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Number of donations by ideology, incumbency, network type and proportion observed
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++   

if(!is.null(LOCAL_DATA)) {
    nominate_data = read_csv(paste0(LOCAL_DATA, 'nominate_prez_data.csv'))
    candidate_meta_data = read_csv(paste0(LOCAL_DATA, 'VLC_16_full.csv')) %>%
        dplyr::select(Actor_ID, Incum) %>%
        rename(candidate = Actor_ID, incumbent = Incum) %>%
        mutate(incumbent = ifelse(incumbent == "I", 1, 0))
} else {
    box_auth()
    nominate_data = box_read_csv(file_id = '311590876251') %>%
        select(os_id, nominate_dim1) %>%
        rename(candidate = os_id, ideology = nominate_dim1)
    # Read 'VLC_16_full.csv' from box
    candidate_meta_data = box_read_csv(file_id = '308095557675', fread = TRUE) %>%
        dplyr::select(Actor_ID, Incum) %>%
        rename(candidate = Actor_ID, incumbent = Incum) %>%
        mutate(incumbent = ifelse(incumbent == "I", 1, 0))
}

## Join with candidate ideology and incumbency data
simulation_cut = left_join(simulation_cut, nominate_data) %>%
    left_join(candidate_meta_data)

# Proportion of contributions to each decile of the ideology distribution
n_breaks = 30
matched = filter(simulation_cut, !is.na(ideology))
matched$ideology_bin = cut(matched$ideology, breaks = n_breaks)

pdat = group_by(matched, network_type, proportion_observed, ideology_bin, 
                cascade_id) %>%
    # get the proportion per ideology bin for each simulation iteration
    summarize(n = n()) %>%
    group_by(network_type, proportion_observed, cascade_id) %>%
    mutate(prop = n / n_donations) %>%
    ## get mean, lo, hi accross simulation iterations
    group_by(network_type, proportion_observed, ideology_bin) %>%
    summarize(mean = mean(prop), 
              lo = quantile(prop, 0.025), 
              hi = quantile(prop, 0.975))

## True donation distributon
dd = as.data.frame(donation_cascades) %>%
    left_join(nominate_data, by = c('cascade_id' = 'candidate')) %>%
    left_join(candidate_meta_data, by = c('cascade_id' = 'candidate')) %>%
    tbl_df() %>% 
    filter(!is.na(ideology))
dd$ideology_bin = cut(dd$ideology, breaks = n_breaks)
dd = group_by(dd, ideology_bin) %>%
    summarize(n = n(),
              prop = n / n_donations)
i = 1
for(p in unique(pdat$proportion_observed)) {
    for(nt in unique(pdat$network_type)) {
        if(i == 1) {
            out = dd
            out$proportion_observed = p
            out$network_type = nt
        } else {
            x = dd
            x$proportion_observed = p
            x$network_type = nt
            out = rbind(out, x)
        }
        i = i + 1
    }
}
dd = out

ggplot(pdat, aes(x = ideology_bin, color = network_type)) +
    geom_point(data = dd, aes(x = ideology_bin, y = prop), color = pe$colors[1],
               alpha = 0.6, size = 1.5) +
    geom_line(data = dd, aes(x = ideology_bin, y = prop, group = 1), 
              color = pe$colors[1], alpha = 0.6) +
    geom_point(aes(y = mean), size = 1.5) +
    geom_segment(aes(y = lo, yend = hi, xend = ideology_bin)) +
    facet_wrap(~network_type + proportion_observed) +
    scale_color_manual(values = pe$colors[-1], guide = FALSE) +
    pe$theme +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) +
    ylab('Within group proportion') + xlab('Ideology')
ggsave('paper/figures/donations_ideology_norm_by_all_donations.png', 
       width = pe$p_width, height = 0.7 * pe$p_width)


# By incumbency status
matched = filter(simulation_cut, !is.na(incumbent))

pdat = group_by(matched, network_type, proportion_observed, incumbent,  
                cascade_id) %>%
    # get the proportion per ideology bin for each simulation iteration
    summarize(n = n()) %>%
    group_by(network_type, proportion_observed, cascade_id) %>%
    mutate(prop = n / sum(n)) %>%
    mutate(incumbent = as.factor(incumbent)) 

## True donation distributon
dd = as.data.frame(donation_cascades) %>%
    left_join(nominate_data, by = c('cascade_id' = 'candidate')) %>%
    left_join(candidate_meta_data, by = c('cascade_id' = 'candidate')) %>%
    tbl_df() %>% 
    filter(!is.na(incumbent)) %>%
    group_by(incumbent) %>%
    summarize(n = n(),
              prop = n / nrow(.)) %>%
    mutate(incumbent = as.factor(incumbent))

pdat$network_type[pdat$network_type == 'directional_networks'] = 'directional'
pdat$network_type[pdat$network_type == 'spatial_networks'] = 'spatial'
pdat$network_type[pdat$network_type == 'netinf_network'] = 'netinf'

ggplot(pdat, aes(x = network_type, y = prop, color = incumbent,
                 linetype = incumbent)) +
    geom_boxplot() +
    geom_hline(data = dd, aes(yintercept = prop, color = incumbent), 
               linetype = 2) +
    scale_color_manual(values = pe$colors, name = '', 
                       labels = c('Non-Incumbent', 'Incumbent')) +
    scale_linetype(name = "", labels = c('Non-Incumbent', 'Incumbent')) +
    pe$theme + ylab('Proportion') + xlab('Network Type') +
    facet_wrap(~proportion_observed) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))
ggsave('../paper/figures/donations_incumbent.png', 
       width = pe$p_width, height = 0.7 * pe$p_width)