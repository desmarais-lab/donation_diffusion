library(tidyverse)
library(doParallel)
library(data.table)
library(Zelig)
library(ROCR)
source('plot_theme.R')


# Generate the dataset 

## Load the donation data that was used to infer the network and the full network
load('../data/results/2016_output.RData')
df <- data.table(output$data)[, Donor_ID, Recip_ID]
nw <- data.table(output$network)
nw <- nw[, origin_node, destination_node] # Don't set the key here! Will mess up the order 
setkey(df, Donor_ID, Recip_ID)
    
## Generate Grid of all Donor - Recipient pairs
donors <- unique(df$Donor_ID)
recipients <- unique(df$Recip_ID)
grid <- data.table(expand.grid(donors, recipients))
colnames(grid) <- c("Donor_ID", "Recip_ID")

# Generate the outcome variable (1 if there is a donation form i->j 0 otherwise)
df$y <- 1
setkey(grid, Donor_ID, Recip_ID)
grid <- df[grid]
grid[is.na(y), y := 0]

## Create the train / test indicator (stratified by the outcome variable)
## The training set is balanced (model will be corrected with weighting)
## The test set is reflecting the original proportions of the data
n_1 <- nrow(df)
n_1_train <- floor(n_1 * 0.7)
n_1_test <- ceiling(n_1 * 0.3)
n_0 <- nrow(grid) - n_1
n_0_train <- n_1_train
n_0_test <- floor((n_0 / n_1) * n_1_test)

set.seed(34178784)
grid[, training_set := rep(-1L, nrow(grid))]
### The 1's will be split up completely between train and test
grid[y == 1, training_set := as.integer(sample(1:n_1, n_1) <= n_1_train)]
### The 0's get equal proportionate test and equal train
#### Generate a selection variable to randomly draw zeros for each train and test
grid[, tt_select := as.integer(rep(NA, nrow(grid)))]
grid[y == 0, tt_select := sample(1:n_0, n_0)]
#### Take the (random) first n_0_test elements and put them in test set
grid[tt_select < n_0_test, training_set := 0]
#### Take the (random) last n_0_train elements and put them in the train set
grid[tt_select >= (n_0 - n_0_train), training_set := 1]
grid[, tt_select := NULL]

# Calculate the number of donations for each recipient
grid[, recip_n_donations := sum(y), by=Recip_ID]

# Calculate the e_ij for each network size

## Function that calculated e_ij for one pair of Donor (i) - Recipient (j)
count_recip_connections <- function(i, matches, combos, donations, edges, unique_o) {

    donor <- as.character(combos[matches[i], "Donor_ID"])
    recip <- as.character(combos[matches[i], "Recip_ID"])
    
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
}

## Remove all rows that are neither in training or test set
grid <- grid[training_set > -1, ]
setkey(grid, Donor_ID)
sizes <- c(1, seq(500, 10000, 500))

## Init parallel cluster
cl <- makeCluster(8, outfile = '')
registerDoParallel(cl)
foreach(i=1:3) %dopar% sqrt(i)

for(size in sizes) {
    cat(paste0('Processing network of size ', size, '\n'))
    # For each network size to be tested, generate indicator if donor in D-R dyad 
    # even occurs in network (if not we know e_ij must be zero)
    this_nw <- nw[1:size, ]
    ud <- unique(c(this_nw$origin_node, this_nw$destination_node))
    donors_in_nw <- data.table(Donor_ID = ud, indicator = 1)
    setkey(donors_in_nw, Donor_ID)
    match <- which(!is.na(donors_in_nw[grid][, indicator]))
    # Calculate e_ij for each matching dyad (row) 
    uo <- unique(this_nw$origin_node)
    setkey(this_nw, origin_node, destination_node)
    out <- foreach(i=1:length(match), .packages=c("data.table"), 
                   .combine=c) %dopar% {
        if(i %% 1e3 == 0) cat(paste(i, 'of', length(match), 
                              'rows processed\n'))  
        count_recip_connections(i, match, grid, df, this_nw, uo)
    }
    #out <- sapply(1:length(match), count_recip_connections, match, grid, df, 
    #              this_nw, uo)
    vname <- paste0('e_ij_', size)
    grid[, counts := rep(0L, nrow(grid))]
    grid[match, counts := out]
    setnames(grid, "counts", vname)
}

save(grid, file = 'edge_likelihood_grid.RData')

# Train models for each network size on training data
train_model <- function(size, balance, data) {
    form <- as.formula(paste0('y ~ recip_n_donations + e_ij_', size))
    mod <- zelig(form, model = 'relogit', tau = balance, 
                 case.control = 'weighting', bias.correct = TRUE, 
                 data = data, cite = FALSE) 
    return(mod)
}

balance <- n_1 / n_0
models <- sapply(sizes, train_model, balance, grid[training_set == 1, ])


# Get the out of sample performance for each model

## inverse logistic function
logit <- function(x) 1 / (1 + exp(-x))

auc <- function(i, data) {
    print(i)
    size <- sizes[i]
    model <- models[[i]]
    y_true <- data[training_set == 0, y]
    vname <- paste0('e_ij_', size)
    x <- as.matrix(cbind(1, data[training_set == 0, get(vname), recip_n_donations]))
    betas <- coef(model)
    #y_pred <- as.integer(logit(x %*% betas) >= 0.5)
    y_pred <- logit(x %*% betas)
    pred <- prediction(y_pred, y_true)
    auc <- as.numeric(performance(pred,"auc")@y.values)
    return(auc)
}

aucs <- data_frame(size = sizes, 
                   auc = sapply(1:length(sizes), auc, grid))

#save.image('edge_likelihood_workspace.RData')

# Calculate the number of isolates (donors not involved in a network)
length(unique(df$Donor_ID)) - length(unique(c(nw$destination_node, nw$origin_node)))

# Plot the aucs
ggplot(aucs, aes(x = size, y = auc)) +
    geom_line() +
    geom_point() +
    plot_theme
ggsave('../paper/figures/auc_edges_2016.png')
