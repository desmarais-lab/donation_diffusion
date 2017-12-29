library(NetworkInference)
library(xtable)
library(tidyverse)
library(doParallel)

result_dir <- '../data/results/'

# Load diffusion networks
res_files <- list.files(result_dir)
networks <- lapply(res_files, function(x) {
    load(paste0(result_dir, x))
    year <- gsub('\\_output.RData$', '', x)
    network <- output$network
    network$year <- year
    return(network)
})
networks <- do.call(rbind, networks) %>% tbl_df()


# Load the donor types for each year
donor_types <- lapply(res_files, function(x) {
    load(paste0(result_dir, x))
    year <- gsub('\\_output.RData$', '', x)   
    df <- output$data
    if(!('Donor_Tp' %in% colnames(df))) {
        df$Donor_Tp <- ifelse(startsWith(df$Donor_ID, "C0"), "PAC", 'IND')
    }
    donors <- group_by(df, Donor_ID) %>%
        summarize(Donor_Tp = Donor_Tp[1],
                  n_records = n())
    donors$year <- year
    return(donors)
})

donor_types <- do.call(rbind, donor_types) %>% 
    tbl_df() %>%
    group_by(Donor_ID) %>%
    summarize(Donor_Tp = Donor_Tp[1],
              dtps = length(unique(Donor_Tp)),
              n_records = sum(n_records),
              n_years = length(unique(year))) %>%
    select(Donor_ID, Donor_Tp)

# Join networks with donor types
networks <- networks %>%
    left_join(donor_types, by = c("origin_node" = "Donor_ID")) %>%
    rename(origin_type = Donor_Tp) %>%
    left_join(donor_types, by = c("destination_node" = 'Donor_ID')) %>%
    rename(destination_type = Donor_Tp) %>%
    mutate(edge_type = paste(origin_type, destination_type, sep = '_'))

# Get distributions
edge_types_year <- group_by(networks, year) %>%
    summarize(PAC_PAC = sum(edge_type == "PAC_PAC") / n(),
              PAC_IND = sum(edge_type == "PAC_IND") / n(),
              IND_PAC = sum(edge_type == "IND_PAC") / n(),
              IND_IND = sum(edge_type == "IND_IND") / n()
              ) %>%
    gather(Measure, Proportion, -year)

ggplot(filter(edge_types_year, !(year %in% c("20165000", "2004")))) +
    geom_point(aes(x = as.integer(year), y = Proportion, color = Measure, 
                   shape = Measure), size = 3) +
    geom_line(aes(x = as.integer(year), y = Proportion, color = Measure, 
                   linetype = Measure)) +
    scale_x_continuous(name = "Year", 
                       breaks = as.integer(unique(edge_types_year$year))) +
    theme_bw()
ggsave(filename = '../paper/figures/edge_types_years.png')


# Trees
# 
trees <- lapply(res_files, function(x) {
    load(paste0(result_dir, x))
    year <- gsub('\\_output.RData$', '', x)
    tree <- output$tree
    tree$year <- year
    return(tree)
})

trees <- do.call(rbind, trees) %>% tbl_df()

# Join networks and trees

nws <- filter(networks) %>% 
    select(origin_node, destination_node, edge_type, year)

trees <- left_join(trees, nws, by = c("parent" = "origin_node", 
                                      "child" = "destination_node",
                                      "year" = "year"))

edge_use <- filter(trees, !is.na(edge_type)) %>% 
    group_by(parent, child) %>% 
    summarize(count_in_trees = n(),
              year = year[1])
ggplot(edge_use) +
    geom_histogram(aes(x = count_in_trees), color = "white") +
    facet_wrap(~ year) +
    scale_x_log10() +
    theme_bw()
ggsave(filename = "../paper/figures/count_in_trees_hist.png")

# Join back to network df, to revover ordering
networks <- left_join(networks, edge_use, by = c("origin_node" = "parent",
                                                 "destination_node" = "child",
                                                 "year" = "year")) %>%
    group_by(year) %>%
    mutate(rank = row_number())


# How often is the nth edge used in a tree
ggplot(networks) +
    geom_segment(aes(x = rank, xend = rank,  y = 0, yend = count_in_trees)) +
    facet_wrap(~year, scales = "free") +
    xlab("Edge Rank (0 is first)") +
    ylab("Number of trees edge is used in") +
    theme_bw()
ggsave(filename = '../paper/figures/edges_in_trees.png')





################################################################################
################################################################################
################################################################################
# Find optimal number of edges (per network computation)

load(paste0(result_dir, res_files[1]))
df_1 <- output$data
nw_1 <- output$network
nw_1$indicator <- 1

# Grid of all donor recipient pairs
donors <- unique(df_1$Donor_ID)
recipients <- unique(df_1$Recip_ID)
grid_1 <- tbl_df(expand.grid(donors, recipients))
colnames(grid_1) <- c('donor', 'recipient')

# Generate the outcome variable (1 if there is a donation form i->j 0 otherwise)
grid_1 <- select(df_1, Donor_ID, Recip_ID) %>%
    mutate(y = 1) %>%
    right_join(grid_1, by = c('Donor_ID' = 'donor', 'Recip_ID' = 'recipient')) %>%
    ungroup()
grid_1$y[is.na(grid_1$y)] <- 0

donation_data <- select(df_1, Donor_ID, Recip_ID)

count_recip_connections_old <- function(i, grd, donation_data, network_data) {
    donor <- as.character(grd[i, 1])
    recip <- as.character(grd[i, 2])
    # Get every donor who gave to recip
    donor_connections <- filter(network_data, origin_node == donor |
                                destination_node == donor)
    if(nrow(donor_connections) == 0) return(0)
    
    # Get the unique connections to 'donor' in the network
    a <- select(donor_connections, origin_node) %>% 
        filter(origin_node != donor) 
    b <- select(donor_connections, destination_node) %>% 
        filter(destination_node != donor) 
    unique_connections <- unique(c(a[,1], b[, 1]))
    
    # Get the number of these unique connections that also gave to recip 
    n_out <- filter(donation_data, Recip_ID == recip, 
                    Donor_ID %in% unique_connections) %>% nrow
    return(n_out) 
}

## SO example
n = 1e6
DT = data.table(A = sample(letters, n, replace = TRUE), 
                B = sample(letters, n, replace = TRUE), value = 1:n)
setkey(DT, A, B)
uA <- unique(DT[, A])

library(microbenchmark)
Union = function(){
   mya = DT["y", which=TRUE]
   myb = DT["y", which=TRUE]
   DT[union(mya,myb)] 
} 
microbenchmark(
    "reduce" = DT[DT[, Reduce('|', lapply(.SD, '==', 'y')), .SDcols = A:B]],
    "rbind" = rbind(DT[.(uA, "y"), nomatch=0], DT[.("y"), nomatch=0]),
    "union" = Union()
)
