#devtools::install_github('desmarais-lab/NetworkInference')
library(NetworkInference)
library(tidyverse)

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


candidates = names(donation_cascades$cascade_nodes)[1:2]

# Load required data
load('../data/casc_sim_data.RData')

# Get parameters from the netinf model
diffmod_params = attr(models[['netinf_network']], 'diffusion_model_parameters')
diffmod = attr(models[['netinf_network']], 'diffusion_model')

for(candidate in candidates) {
    
    # Get just the candidate cascade
    candidate_cascade = subset_cascade(donation_cascades, candidate)
    min_time = min(candidate_cascade$cascade_times[[1]])

    for(prop in c(0.05, 0.1, 0.2)) {
        
        # Generate the partial cascade 
        n_observed = ceiling(length(candidate_cascade$cascade_times[[1]]) * prop)  
        partial_cascade = subset_cascade_n(cascade = candidate_cascade,
                                           ns = n_observed)
         
        for(i in 1:length(models)) {
            model_name = names(models)[i]
            diffnet = models[[i]]
            
            if(model_name = 'netinf_model') {
                # Simulate 100 iterations from this network
                out = simulate_cascades(diffnet, nsim = 100, 
                                        max_time = global_censoring_time,
                                        partial_cascade = partial_cascade,
                                        params = diffmod_params, 
                                        model = diffmod)
            } else {
                # Simulate 1 iteration from each of the 100 networks 
                out = lapply(diffnet[1:20], simulate_cascades, nsim = 1, 
                             max_time = 720, 
                             partial_cascade = partial_cascade,
                             params = diffmod_params,
                             model = diffmod)
                out = lapply(1:length(out), function(i) {
                    x = out[[i]]
                    x$cascade_id = i
                    return(x)
                })
                out = do.call(rbind, out)
            }
        }   
    }
}


# This function simulates n_sim cascades for each candidate based on the 
# provided diffusion_network
sim_cascade = function(diffusion_network, , 
                       params, diffmod, max_time, n_sim) {
   
    
    partial_cascades = subset_cascade_n(cascades, n_observed)
    casc_ids = names(partial_cascades$cascade_nodes)
    
    sim_out = lapply(casc_ids, function(x) {
        s = Sys.time()
        pc = subset_cascade(partial_cascades, x)
        out = simulate_cascades(netinf.network, nsim = 1000, 
                                partial_cascade = pc, 
                                params = params, model = diffmod,
                                max_time = max_time)
        out$cascade_id = x
        print(Sys.time() - s)
        return(out)
    }) 
     
}

# Timings:
# for 10: ~42s
# for 1000: 

# In chunks of 10 cascades per candidate with 42 seconds per candidate:
# 808 candidates * 100 iterations of 10 * 3 partial cascade conditions * 3 models * 42 seconds:
# Batches of 10: 808 * 100 * 3 * 3 * 42 = 30542400 seconds = 353 days
# Batches of 1000: 808 * 3 * 3 * 3600 = 26179200 seconds = 303 days

    
