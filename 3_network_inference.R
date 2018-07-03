library(tidyverse)
library(data.table)
library(NetworkInference)
library(boxr)

init_params = c(1, 1)
box_auth()

# Read 'data_for_netinf.csv' from box
df = box_read_csv(file_id = '302229425978')

# Fit the network
cascades <- as_cascade_long(df, cascade_node_name = 'Donor_ID', 
                            event_time = 'integer_date', 
                            cascade_id = 'Recip_ID')
df_cascades <- data.table(as.data.frame(cascades))
setkey(df_cascades, "node_name", "cascade_id")
network <- data.frame(origin_node = "", destination_node = "")
convergence <- FALSE
i <- 1
while(!convergence) {
    cat("Iteration", i, "\n")
    res = netinf(cascades, trans_mod = 'log-normal', params = init_params,
                 p_value_cutoff = 0.05, trees = TRUE)
    trees_df <- data.table(res$trees) 
    
    # Join trees_df with the event times for each node in each cascade
    # to get diffusion times 
    setkey(trees_df, "parent", "cascade_id") 
    trees <- trees_df[df_cascades, nomatch=0]
    setnames(trees, "event_time", "parent_time")
    setkey(trees, "child", "cascade_id") 
    trees <- trees[df_cascades, nomatch=0] 
    setnames(trees, "event_time", "child_time")
    trees$diffusion_time = trees$child_time - trees$parent_time
    
    # Calculate new parameter values based on diffusion times in inferred 
    # trees 
    params <- c(mean(log(trees$diffusion_time)), 
                sqrt(stats::var(log(trees$diffusion_time))))
    cat('New parameter values: ', params, '\n')           

    if(identical(res$network, network)){
        convergence = TRUE 
    } 
    network <- res$network
    i = i + 1
    # Save current results
    ## Write files to box in dir 'Strategic_Donors/final_paper_data/'
    box_write(network, 
              filename = paste0('netinf_network', '_iter_', i, '.csv'), 
              write_fun = write_csv, 
              dir_id = '50855821402')
}
