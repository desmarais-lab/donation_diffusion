library(data.table)
library(tidyverse)
library(doParallel)
library(sna)
library(igraph)
library(NetworkInference)
library(boxr)
library(yaml)


config = yaml.load_file('0_config.yml')
#LOCAL_DATA = config$LOCAL_DATA
LOCAL_DATA = NULL

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
# Ideological spreading heatmap
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++   

# For each cascade:
# - create dyads of initial candidates with non-initial candidates
# - assign them to ideology bins
# - color heatmap by average time difference

# Join with donor ideology
sim_don_ideo = vlc_16_full %>%
    filter(Ent_Typ == 'IND', !is.na(ideology)) %>%
    select(Actor_ID, ideology) %>%
    rename(donor_ideology = ideology) %>%
    right_join(simulation_cut, by = c('Actor_ID' = 'node_name')) %>%
    rename(candidate_ideology = ideology)

vals = select(sim_don_ideo, cascade_id, proportion_observed, network_type, 
              candidate) %>%
    distinct() 

one_sim_grid = function(i) {
    s = Sys.time()
    print(i)
    x = as.data.frame(samp_vals[i, ])
    one_sim = filter(sim_don_ideo, cascade_id == x[1, 1], 
                     proportion_observed == x[1, 2], network_type == x[1, 3], 
                     candidate == x[1, 4])
    # The rank of each non-initial donor is from 1 to the number of non-initial 
    # donors (nnz) that is, the initial donors don't occupy a rank (or they all
    # simultaneously occupy the 0th rank).
    nnz = sum(one_sim$event_time != 0)
    one_sim$donation_rank = NA
    one_sim$donation_rank[one_sim$event_time != 0] = 1:nnz / nnz
    sim_grid = expand.grid(one_sim$donor_ideology[one_sim$event_time == 0], 
                one_sim$donor_ideology[one_sim$event_time != 0]) %>% 
        tbl_df() %>%
        mutate(init_ideology = as.character(Var1), 
               recip_ideology = as.character(Var2)) %>%
        select(-Var1, -Var2) %>%
        mutate(rank_difference = rep(one_sim$donation_rank[one_sim$event_time !=0],
                                 each = sum(one_sim$event_time == 0)),
               spread_time = rep(one_sim$event_time[one_sim$event_time != 0],
                                 each = sum(one_sim$event_time == 0)),
               cascade_id = x[1, 1], proportion_observed = x[1, 2],
               network_type = x[1, 3], candidate = x[1, 4])
    t = Sys.time() - s
    return(sim_grid)
    #return(t)
}


## sample cascades (candidate-simulations)
set.seed(562165)
samp_vals = group_by(vals, network_type, proportion_observed) %>%
    #summarize(count = n())
    sample_n(1000)

cl <- makeCluster(12)
registerDoParallel(cl)
sim_grid = foreach(i=1:nrow(samp_vals), 
                   .combine = rbind, 
                   .packages = c("dplyr")) %dopar% {
   one_sim_grid(i) 
} 

if(!is.null(LOCAL_DATA)) {
    save(sim_grid, file = paste0(LOCAL_DATA, 'sim_grid.RData'))
} else {
    box_save(sim_grid, dir_id = '50855821402', file_name = 'sim_grid.RData')
}
stop()

if(!is.null(LOCAL_DATA)) {
    load(paste0(LOCAL_DATA, 'sim_grid_backup.RData'))
} else {
    box_load(file_id = '318951870681')
}

#old simulation output with actual spreading times
box_load(file_id = '318951870681')

# Convert to data.table without copy
setDT(sim_grid)
pdat = sim_grid[network_type %in% c('directional_networks', 
                                    'spatial_networks'), 
                mean(spread_time), by = c('init_ideology', 'recip_ideology',
                                         'proportion_observed', 
                                         'network_type')]

# Sort the (character) ideology bins by their numeric value
levs = unique(c(sim_grid_sample$init_ideology, sim_grid_sample$recip_ideology))
re_ordered = levs[order(as.numeric(sapply(strsplit(levs, '–'), 
                                          function(x) return(x[1]))))]

display_labs = re_ordered[c(1, 5, 10, 15, 20)]

ggplot(na.omit(pdat)) + 
    geom_tile(aes(x = factor(recip_ideology, levels = re_ordered), 
                  y = factor(init_ideology, levels = re_ordered), 
                  fill = V1)) +
    scale_fill_gradientn(colors = rev(rainbow(7)[-7])) +
    scale_x_discrete(breaks = display_labs, labels = display_labs) +
    scale_y_discrete(breaks = display_labs, labels = display_labs) +
    labs(fill='Spread\nTime') +
    ylab('Initial Donor Ideology') + xlab('Receiving Donor Ideology') +
    facet_wrap(~proportion_observed + network_type, ncol = 2) +
    rotate_labels(30)


# Get an upper bound from the simulated networks directly

## Generate one big data.frame with all simulated networks (excluding netinf)
direc = lapply(1:length(casc_sim_data$models$directional_networks), 
               function(i) {
                   x = casc_sim_data$models$directional_networks[[i]]
                   x$sim_iter = i
                   x$model = 'directional'
                   return(x)
               })
spati = lapply(1:length(casc_sim_data$models$spatial_networks), 
               function(i) {
                   x = casc_sim_data$models$spatial_networks[[i]]
                   x$sim_iter = i
                   x$model = 'spatial'
                   return(x)
               })

simulated_networks = rbind(do.call(rbind, direc),
                           do.call(rbind, spati)) %>%
    tbl_df()
    

## Join with ideology data on the individuals
## Join with donor ideology
donor_ideo = read_csv('data/VLC_16_full.csv') %>%
    filter(Ent_Typ == 'IND', !is.na(ideology)) %>%
    select(Actor_ID, ideology) %>%
    rename(origin_ideology = ideology)
## join origin node
simulated_networks = left_join(simulated_networks, 
                               donor_ideo, by = c('origin_node' = 'Actor_ID'))
## join destination node
donor_ideo = rename(donor_ideo, destination_ideology = origin_ideology)
simulated_networks = left_join(simulated_networks, 
                               donor_ideo, by = c('destination_node' = 'Actor_ID'))

## Generate the grid of all ideology bin combinations
n_by_bin = group_by(donor_ideo, destination_ideology) %>%
    summarize(count = n())
ideo_bins = unique(donor_ideo$destination_ideology)
ideo_bin_grid = expand.grid(ideo_bins, ideo_bins) %>%
    tbl_df() %>% rename(origin_ideology = Var1, destination_ideology = Var2) %>%
    left_join(n_by_bin, by = c('origin_ideology' = 'destination_ideology')) %>%
    rename(origin_count = count) %>%
    left_join(n_by_bin, by = c('destination_ideology' = 'destination_ideology')) %>%
    rename(destination_count = count) %>%
    mutate(total_count = origin_count * destination_count) %>%
    select(-origin_count, -destination_count)


## Count how often each combination occurs in the simulated networks
### How many donors are in each bin (potential for bin2bin edges)

n_by_grid = expand.grid(n_by_bin$count, n_by_bin$count)
pdat = left_join(simulated_networks, ideo_bin_grid, 
                 by = c('origin_ideology', 'destination_ideology')) %>%
    group_by(model, origin_ideology, destination_ideology) %>% 
    summarize(prop = n() / total_count[1])

# Sort the (character) ideology bins by their numeric value
levs = unique(c(pdat$origin_ideology, pdat$destination_ideology))
re_ordered = levs[order(as.numeric(sapply(strsplit(levs, '–'), 
                                          function(x) return(x[1]))))]


ggplot(pdat) + 
    geom_tile(aes(y = factor(origin_ideology, levels = re_ordered), 
                  x = factor(destination_ideology, levels = re_ordered),
                  fill = prop)) +
    facet_wrap(~model) + 
    theme(axis.text.x = element_text(angle = 60, hjust = 1))

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Number of donations by ideological bin
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++   

## Join donation data with candidate metadata
dd = donation_df %>%
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
ggsave('paper/figures/donations_incumbent.png', 
       width = pe$p_width, height = 0.7 * pe$p_width)






test = data.frame(a = rep(c(1,1,1,2,2,2,3,3,3), 2),
                  b = rep(c(1,2,3,1,2,3,1,2,3), 2),
                  v = rnorm(18))
p = ggplot(test) + 
    geom_tile(aes(x = a, y = b, fill = v))
