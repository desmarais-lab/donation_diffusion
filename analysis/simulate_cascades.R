#devtools::install_github('desmarais-lab/NetworkInference')
library(NetworkInference)
library(tidyverse)

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

# Load required data
load('../data/casc_sim_data.RData')
candidates = names(donation_cascades$cascade_nodes)
start = (job_id - 1) * n_per_job + 1
end = min(job_id * n_per_job, length(candidates))
candidates = candidates[start:end]

print(length(donation_cascades$node_names))

# Get parameters from the netinf model
diffmod_params = attr(models[['netinf_network']], 'diffusion_model_parameters')
diffmod = attr(models[['netinf_network']], 'diffusion_model')

n_per_mod = 100
props = c(0.05, 0.1, 0.2)
results = vector(mode = 'list', 
                 length = length(candidates) * length(props) * length(models))


# TODO: If re-running this, store the candidate id in a column of the results
# data frame
stop("Fix the todo before running")

set.seed(8567187)
j = 1
for(candidate in candidates) {
    start = Sys.time()
    # Get just the candidate cascade
    candidate_cascade = subset_cascade(donation_cascades, candidate)
    min_time = min(candidate_cascade$cascade_times[[1]])

    for(prop in props) {
        
        # Generate the partial cascade 
        n_observed = ceiling(length(candidate_cascade$cascade_times[[1]]) * prop)  
        partial_cascade = subset_cascade_n(cascade = candidate_cascade,
                                           ns = n_observed)
         
        for(i in 1:length(models)) {
            model_name = names(models)[i]
            diffnet = models[[i]]
            
            if(model_name == 'netinf_network') {
                # Simulate 100 iterations from this network
                out = simulate_cascades(diffnet, nsim = n_per_mod, 
                                        max_time = Inf,
                                        partial_cascade = partial_cascade,
                                        params = diffmod_params, 
                                        model = diffmod, 
                                        nodes = donation_cascades$node_names)
                out$network_type = model_name
                out$proportion_observed = prop
            } else if(model_name %in% c('directional_networks', 
                                        'spatial_networks')) {
                # Simulate 1 iteration from each of the 100 networks 
                out = lapply(diffnet, simulate_cascades, 
                             nsim = n_per_mod / length(diffnet), 
                             max_time = Inf, 
                             partial_cascade = partial_cascade,
                             params = diffmod_params,
                             model = diffmod,
                             nodes = donation_cascades$node_names)
                out = lapply(1:length(out), function(i) {
                    x = out[[i]]
                    x$cascade_id = i
                    x$network_type = model_name
                    x$proportion_observed = prop
                    return(x)
                })
                out = do.call(rbind, out)
            } else {
                stop('unexpected model type')
            }
            results[[j]] = out
            j = j + 1
        }
    }
    cat('Timing for candidate', candidate, ':', Sys.time() - start, '\n')
}

fname = paste0('simulated_cascades_', job_id, '.RData')
save(results, file = fname)
