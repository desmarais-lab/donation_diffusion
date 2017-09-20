library(tidyverse)
library(reshape2)

source('plot_theme.R')
log_scale_breaks <- c(1, 10, 100, 1e3, 1e4, 1e5, 1e6, 1e7, 1e8)

df <- read_csv('../data/pac_contributions.csv')

get_proportion = function(x, value) sum(x == value) / length(x)

# Explore the committees (nodes)
pacs = group_by(df, CMTE_ID) %>%
    summarize(total_donations = sum(TRANSACTION_AMT),
              average_donations = mean(TRANSACTION_AMT),
              median_donations = median(TRANSACTION_AMT),
              donation_variance = var(TRANSACTION_AMT), 
              prop_donation_republican = get_proportion(CAND_PTY_AFFILIATION, 'REP'),
              prop_donation_democrat = get_proportion(CAND_PTY_AFFILIATION, 'DEM'),
              prop_donation_incumbend = get_proportion(CAND_ICI, 'I'),
              prop_donation_challenger = get_proportion(CAND_ICI, 'C'),
              prop_donation_open_seat = get_proportion(CAND_ICI, 'C'),
              n_unique_candidates = length(unique(CAND_ID)),
              n_donations = n()
              ) %>%
    melt(id = 'CMTE_ID') %>%
    tbl_df()

# Cumulative distribution for counts
ggplot(filter(pacs, variable %in% c('n_donations', 'n_unique_candidates')), 
       aes(x = value)) +
    stat_ecdf(geom = "step") + 
    scale_x_log10(breaks = log_scale_breaks) +
    facet_wrap(~variable, nrow=1, scales = "free") +
    ylab('Proportion') + xlab('Count') +
    plot_theme
ggsave('../paper/figures/donation_counts.png', width = p_width, 
       height = 0.5 * p_width)

# Cumulative distribution of all donation variables
ggplot(filter(pacs, variable %in% c("total_donations", 'average_donations', 
                                    'median_donations')), 
       aes(x = value)) +
    stat_ecdf(geom = "step") + 
    #geom_histogram(aes(y = (..count..)/sum(..count..)), color = 'white',
    #               bins = 25) +
    scale_x_log10() +
    facet_wrap(~variable, nrow=1, scales = "free") +
    ylab('Proportion') + xlab('Ammount $') +
    plot_theme
ggsave('../paper/figures/donation_distributions.png', width = p_width, 
       height = 0.33 * p_width)

# Cumulative distribution donation target variables
ggplot(filter(pacs, variable %in% c("prop_donation_democrat", 
                                    'prop_donation_republican', 
                                    'prop_donation_incumbent', 
                                    'prop_donation_challenger',
                                    'prop_donation_open_seat')), 
       aes(x = value)) +
    #geom_histogram(aes(y = (..count..)/sum(..count..)), color = 'white') +
    stat_ecdf(geom = "step") + 
    facet_wrap(~variable, scales = "free") +
    ylab('Proportion') +
    plot_theme
ggsave('../paper/figures/donation_targets_distribution.png', 
       width = p_width, height = p_width)

multi_give <- group_by(df, CMTE_ID, CAND_ID) %>%
    summarize(n_donations = n()) %>%
    arrange(desc(n_donations))

ggplot(multi_give, aes(x = n_donations)) +
    stat_ecdf(geom = "step") + 
    scale_x_log10(breaks = log_scale_breaks) +
    ylab("Proportion") + xlab("# of donations to same candidate") +
    plot_theme
ggsave('../paper/figures/donations_to_same_candidate.png', 
       width = p_width, height = p_width)


# Candidates
candidates <- group_by(df, CAND_ID) %>%
    summarize(average_donations = mean(TRANSACTION_AMT),
              median_donations = median(TRANSACTION_AMT),
              donation_variance = var(TRANSACTION_AMT), 
              total_donations = sum(TRANSACTION_AMT),
              n_unique_donors = length(unique(CMTE_ID)),
              n_donations = n()) %>%
    melt(id = 'CAND_ID') %>%
    tbl_df()

# Cumulative distributions of donation amounts
ggplot(filter(candidates, variable %in% c("total_donations", 'average_donations', 
                                          'median_donations')), 
       aes(x = value)) +
    stat_ecdf(geom = "step") + 
    #geom_histogram(aes(y = (..count..)/sum(..count..)), color = 'white',
    #               bins = 25) +
    scale_x_log10() +
    facet_wrap(~variable, nrow=1, scales = "free") +
    ylab('Proportion') + xlab('Ammount $') +
    plot_theme
ggsave('../paper/figures/donation_distributions_candidates.png', width = p_width, 
       height = 0.33 * p_width)

# Cumulative distribution of number of donors
ggplot(filter(candidates, variable %in% c('n_donations', 'n_unique_donors')), 
       aes(x = value)) +
    stat_ecdf(geom = "step") + 
    scale_x_log10(breaks = log_scale_breaks) +
    facet_wrap(~variable, nrow=1, scales = "free") +
    ylab('Proportion') + xlab('Count') +
    plot_theme
ggsave('../paper/figures/donation_counts_candidates.png', width = p_width, 
       height = 0.5 * p_width)
