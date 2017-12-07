library(NetworkInference)
library(xtable)
library(tidyverse)

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

ggplot(filter(edge_types_year)) +
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
