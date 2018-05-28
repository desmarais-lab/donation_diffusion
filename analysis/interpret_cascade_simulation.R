library(tidyverse)
library(sna)
library(igraph)
library(NetworkInference)
#devtools::install_github('flinder/flindR')
pe = flindR::plot_elements()

SIM_RES_DIR = '../data/cascade_simulation_results/'

# Load all simulation results

outfiles = list.files(SIM_RES_DIR, patter = 'new_*')
i = 1
for(f in outfiles) {
    load(paste0('../data/cascade_simulation_results/', f))
    if(i == 1) out = do.call(rbind, results)
    else out = rbind(out, do.call(rbind, results))
    i = i + 1 
    print(i)
}

simulation_results = tbl_df(out)
save(simulation_results, 
     file = '../data/cascade_simulation_results/compiled_results.RData')
#load('../data/cascade_simulation_results/compiled_results.RData')
cutoff_time = 17166

a = group_by(simulation_results, network_type, proportion_observed, cascade_id, candidate_id) %>% summarize(n = n())
tail(filter(a, network_type == "netinf_network") %>% arrange(candidate_id))

# Look at the cascade lengths without the cutoff
c_lengths = group_by(simulation_results, network_type, proportion_observed, 
                     candidate_id, cascade_id) %>%
    summarize(length = n())
a = group_by(c_lengths, network_type, proportion_observed) %>%
    summarize(length = mean(length))
b = filter(simulation_results, event_time <= cutoff_time) %>%
    group_by(network_type, proportion_observed, candidate_id, cascade_id) %>%
    summarize(length = n()) %>%
    group_by(network_type, proportion_observed) %>%
    summarize(length = mean(length))
out = left_join(a, b, by = c('network_type', 'proportion_observed'),
                suffix = c('_no_cutoff', '_cutoff')) %>%
    gather(condition, mean_length, -network_type, -proportion_observed)
ggplot(out, aes(x = network_type, y = mean_length, 
                shape = condition)) +
    geom_point(size = 3) + pe$theme + scale_color_manual(values = pe$colors) +
    facet_wrap(~proportion_observed) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))
ggsave('~/Dropbox/Public/average_cascade_length_all_candidates.png')


# Join with candidate ideology data
nominate_data = read_csv('../data/nominate_prez_data.csv') %>%
    select(os_id, nominate_dim1)

matched = filter(simulation_results, event_time <= cutoff_time) %>%
    left_join(nominate_data, by = c('candidate_id' = 'os_id')) %>% 
    filter(!is.na(nominate_dim1))

c_lengths = group_by(matched, network_type, proportion_observed, 
                     candidate_id, cascade_id) %>%
    summarize(length = n())
a = group_by(c_lengths, network_type, proportion_observed) %>%
    summarize(length = mean(length))
ggplot(a, aes(x = network_type, y = length)) +
    geom_point(size = 3) + pe$theme + scale_color_manual(values = pe$colors) +
    facet_wrap(~proportion_observed) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))
ggsave('~/Dropbox/Public/average_cascade_length_matched_candidates.png')


# Check why cascades have different lengths
## Function definitions
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

load('../data/casc_sim_data.RData')
candidates = names(donation_cascades$cascade_nodes)
prop = 0.05
candidate = candidates[1]
all_donors = donation_cascades$node_names

   
get_non_reachable = function(candidate, diffnet, all_donors) {
    candidate_cascade = subset_cascade(donation_cascades, candidate)
    n_observed = ceiling(length(candidate_cascade$cascade_times[[1]]) * prop)  
    partial_cascade = subset_cascade_n(cascade = candidate_cascade,
                                       ns = n_observed)
    cat('Length part casc:', length(partial_cascade$cascade_nodes[[1]]), '\n')

    netinf_edgelist = select(diffnet, origin_node, destination_node) %>%
        as.matrix()
    network = graph_from_edgelist(netinf_edgelist)
    donors_in_network = vertex_attr(network)$name
    
    # For each donor in the partial cascade get donors that are non reachable 
    paths = lapply(partial_cascade$cascade_nodes[[1]], function(x) {
        p = try(get.shortest.paths(network, from = x), silent = TRUE)
        if(inherits(p, 'try-error')) return(donors_in_network)
        cannot_be_reached = which(unlist(lapply(p[[1]], length)) == 0)
        out = donors_in_network[cannot_be_reached]
        return(out)
    })
    non_reachable_by_all = Reduce(intersect, paths)
    return(non_reachable_by_all)
}

options(warn=-1)

nw_type_idx = 2
nw_type = names(models)[nw_type_idx]

diffnet = models[[nw_type_idx]][[1]]
donors_in_network = unique(c(diffnet$origin_node, diffnet$destination_node))

for(candidate in candidates) {
    sim_dat = filter(simulation_results, network_type == nw_type,
                     proportion_observed == 0.05, candidate_id == candidate)
    reached_donors = unique(sim_dat$node_name)
    print(length(setdiff(reached_donors, donors_in_network)))
    not_reachable = get_non_reachable(candidate, diffnet, all_donors)
    #print(all(sort(setdiff(donors_in_network, reached_donors)) == sort(not_reachable)))
}

options(warn=0)


# Number of contributions to candidates in each decile of nominate
deciles = quantile(matched$nominate_dim1, seq(0, 1, 0.1))
matched = mutate(matched, nominate_deciles = cut(nominate_dim1, deciles, 
                                                 include.lowest = TRUE))

pdat = group_by(matched, network_type, proportion_observed,
                nominate_deciles, cascade_id, candidate_id) %>%
    summarize(n_donations = n()) 
    group_by(network_type, proportion_observed, nominate_deciles, cascade_id) %>%
    summarize(average_length = mean(n_donations))
    
ggplot(pdat) + 
    geom_boxplot(aes(x = nominate_deciles, y = average_length, 
                     color = network_type)) +
    facet_wrap(~proportion_observed) +
    scale_color_manual(values = pe$colors) +
    pe$theme +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))
ggsave('~/Dropbox/Public/deciles.png', width = 16, height = 10)
