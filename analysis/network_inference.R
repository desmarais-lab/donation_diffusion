devtools::install_github('desmarais-lab/NetworkInference', ref = 'devel')

library(tidyverse)
library(NetworkInference)
source('../data_processing/remove_isolates.R')

# Number of nodes to use for inference
n_nodes <- 1000

# Read the preprocessed data (see `make_netinf_data.R` for details)
df <- read_csv('../data/data_for_netinf.R')

# If there are more than N donors, increase threshold
isolate_threshold <- 15
while(length(unique(df$Donor_ID)) > n_nodes) {
    isolate_threshold <- isolate_threshold + 1
    l <- length(unique(df$Donor_ID))
    cat(paste(l, 'unique Donors increasing isolate threshold to', 
              isolate_threshold, '\n'))
    df <- remove_isolates(df, isolate_threshold)
}

## Descriptives
n_donors <- length(unique(df$Donor_ID))
n_recips <- length(unique(df$Recip_ID))
cat(paste0('Number of donors: ', n_donors, '\n'))
cat(paste0('Number of recipients: ', n_recips, '\n'))

# Fit the network
cascades <- as_cascade_long(df, cascade_node_name = 'Donor_ID', 
                            event_time = 'integer_date', 
                            cascade_id = 'Recip_ID',
                            node_names = unique(df$Donor_ID))

df <- simulate_rnd_cascades(100, n_nodes = 50)
cascades <- as_cascade_long(df, node_names = unique(df$node_name))

# Initialize Lambda
max_times <- sapply(cascades$cascade_times, mean)
min_times <- sapply(cascades$cascade_times, 
                    function(x) mean(na.omit(x-lag(x))))
lambda_min <- 1 / mean(max_times, na.rm = T)
lambda_max <- 1 / mean(min_times, na.rm = T)
lambda_start <- mean(c(lambda_max, lambda_min))


df_cascades <- tbl_df(as.data.frame(cascades))
lambda <- lambda_start

nw <- data.frame(origin_node = "", destination_node = "")

while(TRUE) {
    res <- netinf(cascades, n_edges = 0.5, lambda = lambda, trees = TRUE,
                  quiet = FALSE)
    trees <- left_join(res$trees, df_cascades, by = c("cascade_id" = "cascade_id", 
                                         "parent" = "node_name")) %>% 
        rename(parent_time = event_time) %>% 
        left_join(df_cascades, by = c("cascade_id", "child" = "node_name")) %>%
        rename(child_time = event_time) %>%
        mutate(diffusion_time = child_time - parent_time) %>%
        tbl_df()
    
    new_lambda <- 1 / mean(trees$diffusion_time)
    cat(paste0("New lambda: ", round(lambda, 2), ". Difference is ", 
               lambda - new_lambda, "\n"))   
    if(identical(res$network[, c(1,2)], nw)) break
    nw = res$network[, c(1,2)]
    lambda <- new_lambda
}

# Get the trees with the diffusion times for children and parent nodes


# Estimate lambda from the diffusion times in the estimated trees



output <- list('network' = res$net, 'trees' = res$trees, 'data' = df, 
               'filter_info' = filter_info)

save(output, file = paste0('../data/results/', year, '_donors_', N, '_output.RData'))
