library(tidyverse)
library(boxr)
library(flindR)
pe = plot_elements()

load('../data/casc_sim_data.RData')

donors_in_cascades = donation_cascades$node_names

out = vector(mode = 'list', length = 2)
for(i in 1:length(models)) {
    if(i == 3) m_df = dplyr::select(models[[i]], -improvement, -p_value)
    else m_df = do.call(rbind, models[[i]])
    m_df$type = names(models)[i]
    out[[i]] = m_df
}
ergm_networks = do.call(rbind, out)

donors_in_networks = unique(c(ergm_networks$origin_node, 
                              ergm_networks$destination_node))


## For each donor in the cascade find edges in networks with donors in 
## same cascade earlier
count_relevant_edges_donor = function(i, cascade, networks) {
    donor_edges = filter(ergm_networks, destination_node == cascade[i])
    out = c('directional' = 0, 'spatial' = 0, 'netinf' = 0)
    if(nrow(donor_edges) == 0) return(out)
    donor_edges = filter(donor_edges, 
                         origin_node %in% cascade[(i-1):1]) 
    if(nrow(donor_edges) == 0) return(out)
    counts = group_by(donor_edges, type) %>% 
        summarize(n_edges = n())
    d_edges = filter(counts, type == 'directional_networks')$n_edges
    s_edges = filter(counts, type == 'spatial_networks')$n_edges
    n_edges = filter(counts, type == 'netinf_network')$n_edges
    
    d_edges = ifelse(length(d_edges) == 0, 0, d_edges) 
    s_edges = ifelse(length(s_edges) == 0, 0, s_edges) 
    n_edges = ifelse(length(n_edges) == 0, 0, n_edges) 
    
    out['directional'] = d_edges / ((i - 1) * 100)
    out['spatial'] = s_edges  / ((i - 1) * 100)
    out['netinf'] = n_edges / (i - 1)
    return(out)
}

count_relevant_edges_cascade = function(cascade, networks) {
    cascade = cascade[cascade %in% donors_in_networks]
    out = sapply(1:length(cascade), count_relevant_edges_donor, 
                 cascade = cascade, networks = networks)
    return(apply(out, 1, mean))
}

#proportions = do.call(rbind, 
#                      lapply(1:length(donation_cascades$cascade_nodes), 
#                             function(i) {
#                                 print(i)
#                                 ca = donation_cascades$cascade_nodes[[i]]
#                                 count_relevant_edges_cascade(ca, ergm_networks)
#                                 })) %>%
#    tbl_df()
#proportions$candidate = names(donation_cascades$cascade_nodes)
#save(proportions, file = 'edge_proportions_cache.RData')
load('edge_proportions_cache.RData')

nominate_data = read_csv('../data/nominate_prez_data.csv') %>%
    dplyr::select(os_id, nominate_dim1) %>%
    rename(candidate = os_id, ideology = nominate_dim1)

box_auth()
# Read 'VLC_16_full.csv' from box
candidate_meta_data = box_read_csv(file_id = '255302928759', fread = TRUE) %>%
    dplyr::select(Actor_ID, Incum) %>%
    rename(candidate = Actor_ID, incumbent = Incum) %>%
    mutate(incumbent = ifelse(incumbent == "I", 1, 0))

resdat = left_join(proportions, nominate_data) %>% 
    left_join(candidate_meta_data)

## By ideology
pdat = filter(resdat, !is.na(ideology)) %>%
    dplyr::select(-incumbent, -candidate) %>%
    gather(type, proportion, -ideology) %>%
    mutate(ideology_bin = cut(ideology, breaks = 30)) %>%
    group_by(type, ideology_bin) %>%
    summarize(mean = mean(proportion), 
              lo = quantile(proportion, 0.025), 
              hi = quantile(proportion, 0.975))

ggplot(pdat, aes(x = ideology_bin, color = type)) +
    geom_point(aes(y = mean), size = 1.5) +
    geom_segment(aes(y = lo, yend = hi, xend = ideology_bin)) +
    facet_wrap(~type) +
    pe$theme +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) +
    ylab('Proportion') + xlab('Ideology') +
    scale_color_manual(values = pe$colors[-1], guide = FALSE)
ggsave('~/Dropbox/Public/prop_explained_ideology.png', width = pe$p_width, 
       height = 0.7 * pe$p_width)

## By incumbency
pdat = filter(resdat, !is.na(incumbent)) %>%
    dplyr::select(-ideology, -candidate) %>%
    gather(type, proportion, -incumbent) 
  
ggplot(pdat) +
    geom_boxplot(aes(x = as.factor(incumbent), y = proportion, color = type)) +
    pe$theme +
    scale_color_manual(values = pe$colors[-1]) +
    xlab('Incumbent')
ggsave('~/Dropbox/Public/prop_explained_incumbent.png', width = pe$p_width, 
       height = 0.7 * pe$p_width)
