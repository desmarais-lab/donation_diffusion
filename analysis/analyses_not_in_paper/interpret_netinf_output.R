library(tidyverse)
library(NetworkInference)
library(ggplot2)
source('../data_processing/remove_isolates.R')
source('plot_theme.R')

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Config
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
DONDAT = '../data/data_for_netinf.R' # should be .csv change at some point
RESDIR = '../data/results/'


load_net_file = function(threshold) {
    fl = list.files(RESDIR)
    rel_networks = fl[grepl(paste0('_', threshold, "_"), fl)]
    last_network = rel_networks[order(
        as.integer(
            gsub('.RData', '', sapply(
                strsplit(rel_networks, '_'), function(x) x[5])
                )
            ), decreasing = TRUE
        )[1]]
    load(paste0(RESDIR, last_network))
    return(out)
}

get_network = function(threshold) {
    out = load_net_file(threshold)
    network = out$netinf_out
    network = filter(network, p_value <= 0.01)
    network$threshold = threshold
    return(network)
}

get_n_donors = function(threshold, data) {
    dat = remove_isolates(data, threshold)
    donors = group_by(dat, Donor_ID) %>%
    summarize(total_amount = sum(Amt),
              n_donations = n())  
    return(nrow(donors))
}

# Load the donation data
df = read_csv(DONDAT)

# Load latest version of all networks 
thresholds = c(5, seq(6, 18, 2))
networks = lapply(thresholds, get_network)
networks = tbl_df(do.call(rbind, networks))

# Get the number of unique donors for each base dataset
n_donors_in_data = sapply(thresholds, get_n_donors, data = df)
n_donors_in_data = data_frame(threshold = thresholds, n_donors = n_donors)


pdat = left_join(networks, n_donors, by = "threshold") %>%
    group_by(threshold) %>%
    summarize(n_donors_in_network_origin = length(unique(origin_node)),
              n_donors_in_network_destination = length(unique(destination_node)),
              perc_in_data_origin = n_donors_in_network_origin / n_donors[1],
              n_donors_in_data = n_donors[1],
              n_edges = n())



# Simulation
thres = 50
d = remove_isolates(df, thres)
orig_cascades = as_cascade_long(data = d, cascade_node_name = "Donor_ID", 
                                event_time = "integer_date", 
                                cascade_id = "Recip_ID")
#output = load_net_file(thres)
#nw = output$netinf_out
params = c(4.128071, 1.354032)
nw = netinf(orig_cascades, trans_mod = "log-normal", n_edges = 0.2, 
            max_iter = 10, params = params)

n_cascades = length(unique(d$Recip_ID))
# Get fist adopters
nodes <- data_frame(node_name = unique(c(nw$origin_node, nw$destination_node)))
nodes$node_id = 1:nrow(nodes)

first_adopters = data_frame(node_name = sapply(orig_cascades$cascade_nodes, 
                                               function(x) x[1])) %>%
    left_join(nodes) %>%
    group_by(node_name) %>%
    summarize(count = n()) %>% 
    right_join(nodes) %>%
    mutate(count = ifelse(is.na(count), 0, count),
           prob = count / sum(count)) 

a = data_frame(node_name = unique(nw$origin_node), matched = 1) %>% 
    right_join(first_adopters) %>% filter(is.na(matched))
table(a$count)
sim_df = simulate_cascades(nw, nsim = n_cascades, max_time = 730, 
                           start_probabilities = as.numeric(first_adopters$prob))
sim_casc = as_cascade_long(sim_df)
summary(orig_cascades)
summary(sim_casc)

res_sim = netinf(sim_casc, n_edges = 0.15, params = params, trans_mod = model,
                 max_iter = 1)

orig_casc = orig_cascades
orig_casc$cascade_times = lapply(orig_cascades$cascade_times, 
                                 function(x) x - x[1])

plot(sim_casc, selection = sample(names(sim_casc$cascade_nodes), 10), label_nodes = FALSE)
ggsave('~/Dropbox/Public/simulated_cascades_sample.png')
plot(orig_casc, selection = sample(names(orig_casc$cascade_nodes), 10), label_nodes = FALSE)
ggsave('~/Dropbox/Public/original_cascades_sample.png')
