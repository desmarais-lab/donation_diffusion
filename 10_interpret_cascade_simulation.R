library(data.table)
library(tidyverse)
library(doParallel)
library(sna)
library(igraph)
library(NetworkInference)
library(boxr)
library(yaml)
library(fANCOVA)

config = yaml.load_file('0_config.yml')
LOCAL_DATA = config$LOCAL_DATA
FIGURES_PATH = config$FIGURES_PATH

#devtools::install_github('flinder/flindR')
pe = flindR::plot_elements()

## Load all simulation results

#SIM_RES_DIR = 'data/cascade_simulation_results/'
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
if(!is.null(LOCAL_DATA)) {
    load(paste0(LOCAL_DATA, 'cascade_simulation_results/compiled_results.RData'))
    load(paste0(LOCAL_DATA, 'casc_sim_data.RData'))
} else {
    box_auth()
    box_load(file_id = '311588038097')
    box_load(file_id = '311571533736')
}

donation_cascades = casc_sim_data$donation_cascades

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
# Average error in donation rank from simulations from different networks
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++   

if(!is.null(LOCAL_DATA)) {
    nominate_data = read_csv(paste0(LOCAL_DATA, 'nominate_prez_data.csv')) %>%
        select(os_id, nominate_dim1) %>%
        rename(candidate = os_id, ideology = nominate_dim1)
    vlc_16_full = read_csv(paste0(LOCAL_DATA, 'VLC_16_full.csv'))
    candidate_meta_data = vlc_16_full %>%
        dplyr::select(Actor_ID, Incum) %>%
        rename(candidate = Actor_ID, incumbent = Incum) %>%
        mutate(incumbent = ifelse(incumbent == "I", 1, 0))
} else {
    box_auth()
    nominate_data = box_read_csv(file_id = '311590876251') %>%
        select(os_id, nominate_dim1) %>%
        rename(candidate = os_id, ideology = nominate_dim1)
    # Read 'VLC_16_full.csv' from box
    vlc_16_full = box_read_csv(file_id = '308095557675', fread = TRUE)
    candidate_meta_data = vlc_16_full %>%
        dplyr::select(Actor_ID, Incum) %>%
        rename(candidate = Actor_ID, incumbent = Incum) %>%
        mutate(incumbent = ifelse(incumbent == "I", 1, 0))
}

## Join with candidate ideology and incumbency data
simulation_cut = left_join(simulation_cut, nominate_data) %>%
    left_join(candidate_meta_data)


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Explore simulation results
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++   

# Join with donor ideology
sim_don_ideo = vlc_16_full %>%
    filter(Ent_Typ == 'IND', !is.na(ideology)) %>%
    select(Actor_ID, ideology) %>%
    rename(donor_ideology = ideology) %>%
    right_join(simulation_cut, by = c('Actor_ID' = 'node_name')) %>%
    rename(candidate_ideology = ideology)

#if(!is.null(LOCAL_DATA)) {
#    save(sim_don_ideo, file = 'simulation_output_w_ideology.RData')
#} else {
#    box_save(sim_don_ideo, dir_id = '50855821402', 
#             file_name = 'simulation_output_w_ideology.RData')
#}
#load("data/simulation_output_w_ideology.RData")

# create an id for each simulated cascade
sim_don_ideo$simulation_id = paste(sim_don_ideo$candidate, 
                                    sim_don_ideo$cascade_id,
                                    sim_don_ideo$proportion_observed,
                                    sim_don_ideo$network_type, sep="_")

# list of unique ids
u_simids = unique(sim_don_ideo$simulation_id)

# convert ideology to numeric score
char_ideos = unique(sim_don_ideo$donor_ideology)
ideo_map = data.frame(
    char = as.character(char_ideos),
    num = sapply(char_ideos, function(x) {
        mean(as.numeric(unlist(strsplit(x, '–'))))
    }),
    stringsAsFactors = FALSE
)

numeric_donor_ideology = numeric(length(sim_don_ideo$donor_ideology))
for(i in 1:nrow(ideo_map)) {
    ch = ideo_map[i, 1]
    nu = ideo_map[i, 2]
    numeric_donor_ideology[sim_don_ideo$donor_ideology == ch] = nu
}

# calculate ideological extremeness
sim_don_ideo$ideo_extreme = abs(numeric_donor_ideology-50)

# store donor ideology
sim_don_ideo$numeric_donor_ideology = numeric_donor_ideology

set.seed(123)
n = 100000
samp_simids = sample(u_simids, n)
cascade_data = filter(sim_don_ideo, simulation_id %in% samp_simids) %>%
    group_by(simulation_id) %>%
    summarize(
        n_donors = n(),
        seeds = sum(event_time == 0),
        seed_extremeness = mean(ideo_extreme[event_time == 0]),
        nonseed_extremeness = mean(ideo_extreme[event_time > 0]),
        nonseed_moderates = mean(ideo_extreme[event_time > 0] < 20),
        network_type = network_type[1],
        candidate = candidate[1],
        proportion = proportion_observed[1]
    )

# Plots

## limit to cascades with at least 5 donors and dont plot netif network
pdat = filter(cascade_data, n_donors > 4, network_type != 'netinf_network')

## Mean non-seed extremeness
cors = group_by(pdat, network_type) %>%
    summarize(correlation = cor(seed_extremeness, nonseed_extremeness))
p = ggplot(pdat, aes(x = seed_extremeness, y = nonseed_extremeness)) +
    geom_point(size = 0.8,
               alpha = 0.4,
               color = 'grey') +
    geom_smooth(method = 'loess',
                color = 'black',
                alpha = 0.8) +
    geom_label(data = cors, aes(
        x = 20,
        y = 25,
        label = paste0("Correlation = ", round(correlation, 2))
    )) +
    geom_smooth(method = 'lm', alpha = 0.8) +
    facet_wrap(~ network_type) +
    pe$theme +
    xlab('Mean seed extremeness') +
    ylab('Mean non-seed extremeness')
ggsave(
    p,
    file = paste0(FIGURES_PATH, 'simresults_mean_nonseeds.png'),
    width = pe$p_width,
    height = 0.5 * pe$p_width
)

## Non-seed moderates
cors = group_by(pdat, network_type) %>%
    summarize(correlation = cor(seed_extremeness, nonseed_moderates))
p = ggplot(pdat, aes(x = seed_extremeness, y = nonseed_moderates)) +
    geom_point(size = 0.8,
               alpha = 0.4,
               color = 'grey') +
    geom_smooth(method = 'loess',
                color = 'black',
                alpha = 0.8) +
    geom_label(data = cors, aes(
        x = 20,
        y = 0.5,
        label = paste0("Correlation = ", round(correlation, 2))
    )) +
    geom_smooth(method = 'lm', alpha = 0.8) +
    facet_wrap(~ network_type) +
    pe$theme +
    xlab('Mean seed extremeness') +
    ylab('Proportion non-seed moderates')
ggsave(
    p,
    file = paste0(FIGURES_PATH, 'simresults_moderates_nonseeds.png'),
    width = pe$p_width,
    height = 0.5 * pe$p_width
)