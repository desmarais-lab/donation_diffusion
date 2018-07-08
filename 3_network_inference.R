library(tidyverse)
library(data.table)
library(NetworkInference)
library(boxr)
library(yaml)

init_params = c(1, 1)
config = yaml.load_file('0_config.yml')
LOCAL_DATA = 'data/'
#LOCAL_DATA = NULL

# Read the data prepared for netinf (see 1_make_netinf_data.R) either from 
# Box or LOCAL_DATA
data_input_file = paste0('data_for_netinf_threshold_', config$ISOLATE_THRESHOLD, 
                         '.csv')
if(!is.null(LOCAL_DATA)) {
    df = read_csv(paste0(LOCAL_DATA, data_input_file))
} else {
    if(is.null(config[[data_input_file]])) stop('run 1_make_netinf_data.R first.')
    box_auth()
    # Read 'data_for_netinf.csv' from box
    df = box_read_csv(file_id = config$data_for_netinf_threshold_8.csv)
}

# Fit the network
cascades = as_cascade_long(df, cascade_node_name = 'Donor_ID', 
                            event_time = 'integer_date', 
                            cascade_id = 'Recip_ID')
df_cascades = data.table(as.data.frame(cascades))
setkey(df_cascades, "node_name", "cascade_id")
network = data.frame(origin_node = "", destination_node = "")
convergence = FALSE
i = 1
while(!convergence) {
    cat("Iteration", i, "\n")
    res = netinf(cascades, trans_mod = 'log-normal', params = init_params,
                 p_value_cutoff = 0.05, trees = TRUE)
    trees_df = data.table(res$trees) 
    
    # Join trees_df with the event times for each node in each cascade
    # to get diffusion times 
    setkey(trees_df, "parent", "cascade_id") 
    trees = trees_df[df_cascades, nomatch=0]
    setnames(trees, "event_time", "parent_time")
    setkey(trees, "child", "cascade_id") 
    trees = trees[df_cascades, nomatch=0] 
    setnames(trees, "event_time", "child_time")
    trees$diffusion_time = trees$child_time - trees$parent_time
    
    # Calculate new parameter values based on diffusion times in inferred 
    # trees 
    params = c(mean(log(trees$diffusion_time)), 
                sqrt(stats::var(log(trees$diffusion_time))))
    cat('New parameter values: ', params, '\n')           

    if(identical(res$network, network)){
        convergence = TRUE 
    } 
    network = res$network
    # Save current results
    ## Write files to box in dir 'Strategic_Donors/final_paper_data/'
    fname = paste0('netinf_network_threshold_', config$ISOLATE_THRESHOLD,
                   '_pval_', config$P_VALUE, '_iter_', i, '.csv')
    ref = box_write(network, 
                    filename = , 
                    write_fun = write_csv, 
                    dir_id = '50855821402')
    i = i + 1
    ## Store file reference of latest iteration to config (w/o iteration) 
    fname = paste0('netinf_network_threshold_', config$ISOLATE_THRESHOLD,
                   '_pval_', config$P_VALUE, '.csv')
    config[[fname]] = ref
}