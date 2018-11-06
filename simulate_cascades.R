#devtools::install_github('desmarais-lab/NetworkInference')
library(NetworkInference)
library(tidyverse)
library(boxr)

config = yaml.load_file('0_config.yml')
LOCAL_DATA = config$LOCAL_DATA

job_id = as.integer(commandArgs(trailingOnly = TRUE)[1])
n_per_job = as.integer(commandArgs(trailingOnly = TRUE)[2])

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

sim_casc_out_degree = function(diffnet, nsim, params, model, nodes, 
                               cand_cascade, n_observed) {
   out_degrees = group_by(diffnet, origin_node) %>%
       summarize(out_degree = n()) %>%
       filter(origin_node %in% cand_cascade$cascade_nodes[[1]]) %>%
       mutate(weight = out_degree / sum(out_degree))
   
   if(nrow(out_degrees) == 0) return(NULL) # all observed donors are isolates
     
   seed_nodes = sample(x = out_degrees$origin_node, 
                       size = min(n_observed, length(out_degrees$origin_node)), 
                       prob = out_degrees$weight, replace = FALSE)
   partial_cascade = list(cascade_nodes = list(seed_nodes), 
                          cascade_times = list(rep(0, length(seed_nodes))),
                          node_names = nodes)
   class(partial_cascade) = 'cascade'
   
   out = simulate_cascades(diffnet, nsim = nsim, partial_cascade = partial_cascade,
                           params = params, model = model, nodes = nodes, 
                           max_time = Inf)
   return(out)
}

# Load required data
if(is.null(LOCAL_DATA)) {
    box_auth()
    load('data/casc_sim_data.RData')
}
else box_load(file_id = '311571533736')

candidates = names(casc_sim_data$donation_cascades$cascade_nodes)
start = (job_id - 1) * n_per_job + 1
end = min(job_id * n_per_job, length(candidates))
candidates = candidates[start:end]

# Get parameters from the netinf model
diffmod_params = attr(casc_sim_data$models[['netinf_network']], 
                      'diffusion_model_parameters')
diffmod = attr(casc_sim_data$models[['netinf_network']], 
               'diffusion_model')

n_per_mod = 100
props = c(0.05, 0.1, 0.2)
results = vector(mode = 'list', 
                 length = length(candidates) * length(props) * 
                          length(casc_sim_data$models))


set.seed(8567187)
j = 1
for(candidate in candidates) {
    start = Sys.time()
    # Get just the candidate cascade
    candidate_cascade = subset_cascade(casc_sim_data$donation_cascades, 
                                       candidate)

    for(prop in props) {
        
        # Generate the partial cascade 
        n_observed = ceiling(length(candidate_cascade$cascade_times[[1]]) * prop)  
        #partial_cascade = subset_cascade_n(cascade = candidate_cascade,
        #                                   ns = n_observed)
         
        for(i in 1:length(casc_sim_data$models)) {
            model_name = names(casc_sim_data$models)[i]
            diffnet = casc_sim_data$models[[i]]
            
            if(model_name == 'netinf_network') {
                # Simulate 100 iterations from this network
                class(diffnet) = c('diffnet', 'data.frame')
                out = sim_casc_out_degree(
                            diffnet = diffnet, nsim = n_per_mod, 
                            params = diffmod_params, 
                            model = diffmod, 
                            nodes = casc_sim_data$donation_cascades$node_names, 
                            cand_cascade = candidate_cascade, 
                            n_observed = n_observed)
                if(!is.null(out)) {
                    out$network_type = model_name
                    out$proportion_observed = prop
                    out$candidate = candidate                   
                }
            } else if(model_name %in% c('directional_networks', 
                                        'spatial_networks')) {
                # Simulate 1 iteration from each of the 100 networks 
                out = lapply(1:length(diffnet), function(k) {
                    x = diffnet[[k]]
                    o = sim_casc_out_degree(diffnet = x, 
                                            nsim = n_per_mod / length(diffnet), 
                                            params = diffmod_params, 
                                            model = diffmod, 
                                            nodes = casc_sim_data$donation_cascades$node_names, 
                                            cand_cascade = candidate_cascade, 
                                            n_observed = n_observed)
                    if(!is.null(o)) {
                        o$cascade_id = k
                        o$network_type = model_name
                        o$proportion_observed = prop
                        o$candidate = candidate                      
                    }
                    return(o)
                })
                # Remove NA outputs
                names(out) = seq_along(out)
                out = Filter(Negate(is.null), out)
                if(length(out) > 0) out = do.call(rbind, out)
                else out = NULL
            } else {
                stop('unexpected model type')
            }
            results[[j]] = out
            j = j + 1
        }
    }
    cat('Timing for candidate', candidate, ':', Sys.time() - start, '\n')
}

fname = paste0('data/cascade_simulation_results/simulated_cascades_', job_id, '.RData')
save(results, file = fname)
