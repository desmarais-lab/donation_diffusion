library(tidyverse)

source('plot_theme.R')

df = read_csv('data/2016_PAC_CONTRIBUTIONS.csv')

get_proportion = function(x, value) sum(x == value) / length(x)

# Explore the committees (nodes)
pacs = group_by(df, CMTE_ID) %>%
    summarize(total_donations = sum(DONOR_AMT),
              average_donations = mean(DONOR_AMT),
              median_donations = median(DONOR_AMT),
              donation_variance = var(DONOR_AMT), 
              prop_donation_republican = get_proportion(PARTY, 'REP'),
              prop_donation_democrat = get_proportion(PARTY, 'DEM'),
              prop_donation_incumbend = get_proportion(INCUMBENCY, 'I'),
              prop_donation_challenger = get_proportion(INCUMBENCY, 'C'),
              prop_donation_open_seat = get_proportion(INCUMBENCY, 'C'),
              n_unique_candidates = length(unique(CAND_ID)),
              n_donations = n()
              ) %>%
    melt(id = 'CMTE_ID') %>%
    tbl_df()

# Cumulative distribution for counts
ggplot(filter(pacs, variable %in% c('n_donations', 'n_unique_candidates')), 
       aes(x = value)) +
    stat_ecdf(geom = "step") + 
    scale_x_log10() +
    facet_wrap(~variable, nrow=1, scales = "free") +
    ylab('Proportion') + xlab('Count') +
    plot_theme
ggsave('paper/figures/donation_counts.png', width = p_width, 
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
ggsave('paper/figures/donation_distributions.png', width = p_width, 
       height = 0.5 * p_width)

# Cumulative distribution donation target variables
ggplot(filter(pacs, variable %in% c("prop_donation_democrat", 
                                    'prop_donation_republican', 
                                    'prop_donation_incumbent', 
                                    'prop_donation_challenger',
                                    'prop_donation_open_seat')), 
       aes(x = value)) +
    #geom_histogram(aes(y = (..count..)/sum(..count..)), color = 'white') +
    stat_ecdf(geom = "step") + 
    facet_wrap(~variable, nrow=1, scales = "free") +
    ylab('Proportion') +
    theme_bw()
ggsave('paper/figures/donation_targets_distribution.png', 
       width = p_width, height = 0.5 * p_width)