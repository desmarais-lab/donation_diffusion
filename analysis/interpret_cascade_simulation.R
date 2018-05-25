library(tidyverse)

source('plot_theme.R')
SIM_RES_DIR = '../data/cascade_simulation_results/'

# Load all simulation results

## I forgot to store the candidate ID in each dataframe so these have to be 
## reconstructed 
#load('../data/casc_sim_data.RData')
#candidates = names(donation_cascades$cascade_nodes)
#n_per_job = 10 # Look up from make_cascade_simulation_job.py
#n_props = 3
#
#outfiles = list.files(SIM_RES_DIR, patter = '*.RData')
#i = 1
#for(f in outfiles) {
#    job_id = as.integer(unlist(strsplit(unlist(strsplit(f, '_'))[3], '\\.'))[1])
#    start = (job_id - 1) * n_per_job + 1
#    end = min(job_id * n_per_job, length(candidates))
#    cands = rep(candidates[start:end], each = n_props * 3)
#    load(paste0('../data/cascade_simulation_results/', f))
#    new_results = lapply(1:length(results), function(j) {
#        x = results[[j]]
#        x$candidate_id = cands[j] 
#        return(x)
#    })
#    if(i == 1) out = do.call(rbind, new_results)
#    else out = rbind(out, do.call(rbind, new_results))
#    i = i + 1 
#    print(i)
#}
#simulation_results = tbl_df(out)
#
#save(simulation_results, 
#     file = '../data/cascade_simulation_results/compiled_results.RData')
load('../data/cascade_simulation_results/compiled_results.RData')
cutoff_time = 17166

# Join with candidate ideology data
simulation_results = read_csv('../data/nominate_prez_data.csv') %>%
    select(os_id, nominate_dim1) %>%
    right_join(simulation_results, by = c('os_id' = 'candidate_id')) %>% 
    filter(!is.na(nominate_dim1), event_time <= cutoff_time)

# Number of contributions to candidates in each decile of nominate
deciles = quantile(simulation_results$nominate_dim1, seq(0, 1, 0.1))
simulation_results = mutate(simulation_results, 
                            nominate_deciles = cut(nominate_dim1, 
                                                   deciles, 
                                                   include.lowest = TRUE))
pdat = group_by(simulation_results, network_type, proportion_observed,
                nominate_deciles, cascade_id) %>%
    summarize(n_donations = n())
    
    
ggplot(pdat) + 
    geom_boxplot(aes(x = nominate_deciles, y = n_donations, 
                     color = network_type)) +
    facet_wrap(~proportion_observed) +
    scale_color_manual(values = cbPalette) +
    plot_theme +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))
ggsave('deciles.png', width = 16, height = 10)
