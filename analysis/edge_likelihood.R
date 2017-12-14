library(tidyverse)
library(doParallel)
library(data.table)
library(zelig)
source('plot_theme.R')

# Find optimal number of edges (per network computation)

count_recip_connections <- function(i, combos, donations, edges, unique_o) {
    #start <- Sys.time()
    donor <- as.character(combos[i, "Donor_ID"])
    recip <- as.character(combos[i, "Recip_ID"])
    
    # Get all edges that involve 'donor'
    donor_connections <- rbind(edges[.(unique_o, donor), nomatch=0], 
                               edges[.(donor), nomatch=0]) 
    
    # Get the unique connections to 'donor' in the network
    unique_connections <- unique(c(donor_connections$destination_node, 
                                   donor_connections$origin_node)) 
    # remove self-reference
    unique_connections <- unique_connections[unique_connections != donor]
    
    # Get the number of these unique connections that also gave to recip 
    n_out <- nrow(donations[.(unique_connections, recip), nomatch=0])
    return(n_out) 
    #return(Sys.time() - start)
}

generate_ds <- function(n_edges, res_file){
    
    load(paste0(result_dir, res_file))
    df <- data.table(output$data)[, Donor_ID, Recip_ID]
    nw_in <- output$network[1:n_edges, ]
    nw <- data.table(nw_in)[, origin_node, destination_node]
    # Set data table keys (ordered indices for binary search)
    setkey(df, Donor_ID, Recip_ID)
    setkey(nw, origin_node, destination_node)
    
    # Grid of all donor recipient pairs
    donors <- unique(df$Donor_ID)
    recipients <- unique(df$Recip_ID)
    grid <- data.table(expand.grid(donors, recipients))
    colnames(grid) <- c("Donor_ID", "Recip_ID")
    
    # Generate the outcome variable (1 if there is a donation form i->j 0 otherwise)
    df$y <- 1
    setkey(grid, Donor_ID, Recip_ID)
    grid <- df[grid]
    grid[is.na(y), y := 0]
    
    # Sample zero's in the grid
    n_non_zero <- sum(grid$y) 
    selection <- c(which(grid$y == 1), sample(which(grid$y == 0), n_non_zero))
    ## Save the original balance
    balance <- n_non_zero / sum(grid$y == 0)
    grid <- grid[selection, ] 
     
    # Match with network data to get shortcut on donors that are not in the nw
    # Donors that occur in grid but not in nw must have e_ij = 0
    
    ## Generate table with all unique donors occuring in nw
    ud <- unique(c(nw$origin_node, nw$destination_node))
    donors_in_nw <- data.table(Donor_ID = ud, indicator = 1)
    setkey(donors_in_nw, Donor_ID)
    setkey(grid, Donor_ID)
    grid <- donors_in_nw[grid]
    grid[, indicator := !is.na(indicator)] 
    colnames(grid) <- c("Donor_ID", "match","Recip_ID", "y")
    
    # Calculate the edge_counts on the donor-recip dyads where the donor occurs
    # in the network 
    grid_matches <- which(grid$match)
    uo <- unique(nw$origin_node)
    out <- as.integer(sapply(grid_matches, count_recip_connections, grid, df, 
                             nw, uo))
    # Generate the edge_count variable
    grid$e_ij <- rep(0L, nrow(grid))
    grid[grid_matches, e_ij := out]

    cat(paste('Done with', res_file, n_edges, '\n'))
    return(grid)
}



process_res_file <- function(res_file) {
    n_edges <- c(1, seq(500,10000, 500))
    data_sets <- list()
    for(i in 1:length(n_edges)) {
        data_sets[[i]] <- generate_ds(n_edges[i], res_file)
    }
    return(data_sets)
}




cl <- makeCluster(10, outfile="")
registerDoParallel(cl)

result_dir <- '../data/results/'

# Load diffusion networks
res_files <- list.files(result_dir)

out <- foreach(i=1:length(res_files), 
               .packages = c("tidyverse", "data.table", "Zelig")) %dopar% {
            #print(paste('Processing', res_files[i])) 
            process_res_file(res_files[i])
               }

save(out, file = 'edge_likelihood.RData')
#load('edge_likelihood.RData')
stop('Completed')




    mod <- zelig(y ~ e_ij, model = "relogit", tau = balance, 
                 case.control = c("weighting"),
                 bias.correct = TRUE, data = grid, cite = FALSE)

    aic <- mod$from_zelig_model()$aic


# Get n in grid and n with downsampled 0's
ns_grid <- rep(NA, length(res_files))
ns_down_sampled <- rep(NA, length(res_files))

for(i in 1:length(res_files)) {
    print(i)
    load(paste0(result_dir, res_files[i]))
    df <- data.table(output$data)[, Donor_ID, Recip_ID]
    # Set data table keys (ordered indices for binary search)
    setkey(df, Donor_ID, Recip_ID)
    
    # Grid of all donor recipient pairs
    donors <- unique(df$Donor_ID)
    recipients <- unique(df$Recip_ID)
    grid <- data.table(expand.grid(donors, recipients))
    ns_grid <- nrow(grid)
    colnames(grid) <- c("Donor_ID", "Recip_ID")
    
    # Generate the outcome variable (1 if there is a donation form i->j 0 otherwise)
    df$y <- 1
    setkey(grid, Donor_ID, Recip_ID)
    grid <- df[grid]
    grid[is.na(y), y := 0]
    
    # Sample zero's in the grid
    n_non_zero <- sum(grid$y) 
    selection <- c(which(grid$y == 1), sample(which(grid$y == 0), n_non_zero))
    ## Save the original balance
    balance <- n_non_zero / sum(grid$y == 0)
    grid <- grid[selection, ] 
    
    ns_down_sampled[i] <- nrow(grid)
}

aics <- as_data_frame(do.call(rbind, out))
aics$year <- gsub('\\D', '', res_files)
aics$n_obs_raw <- ns_grid
aics$n_obs_down_sampled <- ns_down_sampled
colnames(aics) <- c(as.character(seq(1, 1e4, 500)), 'year', 'n_obs_raw', 'n_obs_down_sampled')
pdat <- gather(aics, n_edges, AIC, -year, -n_obs_raw, -n_obs_down_sampled)
pdat$n_edges <- as.integer(pdat$n_edges)
pdat$log_likelihood <- (4 - pdat$AIC) / 2
pdat$edge_penalized_aic <- (2 * (pdat$n_edges + 2) - 2 * pdat$log_likelihood) * 1e-5
pdat$bic_ds <- -2 * pdat$log_likelihood + (pdat$n_edges + 2) * log(pdat$n_obs_down_sampled)
pdat$bic_raw <- -2 * pdat$log_likelihood + (pdat$n_edges + 2) * log(pdat$n_obs_raw)
ggplot(pdat) +
    geom_point(aes(x = n_edges, y = edge_penalized_aic), size = 0.7) +
    geom_line(aes(x = n_edges, y = edge_penalized_aic, group = year)) +
    ylab("AIC * 10e-4") +
    facet_wrap(~year) +
    plot_theme
ggsave(filename = '../paper/figures/aics.png')
ggplot(pdat) +
    geom_point(aes(x = n_edges, y = bic_ds), size = 0.7) +
    geom_line(aes(x = n_edges, y = bic_ds, group = year)) +
    ylab("BIC") +
    facet_wrap(~year) +
    plot_theme
ggsave(filename = '../paper/figures/bics.png')
ggplot(pdat) +
    geom_point(aes(x = n_edges, y = bic_raw), size = 0.7) +
    geom_line(aes(x = n_edges, y = bic_raw, group = year)) +
    ylab("BIC") +
    facet_wrap(~year) +
    plot_theme
ggsave(filename = '../paper/figures/bics.png')
