library(tidyverse)
library(NetworkInference)
library(microbenchmark)

parse_date <- function(x) {
    x = as.character(x)
    l = sapply(strsplit(x, ''), length)
    year = substr(x, l-3, l)
    day = substr(x, l-5, l-4)
    month = substr(x, 1, l-6)
    return(as.Date(paste(year, month, day, sep = '/')))
}

df <- read_csv('../data/pac_contributions.csv') %>%
    mutate(date = as.integer(parse_date(TRANSACTION_DT))) %>%
    group_by(CMTE_ID, CAND_ID) %>%
    summarize(date = min(date))

cascades <- as_cascade_long(df, cascade_node_name = 'CMTE_ID', 
                            event_time = 'date', 
                            cascade_id = 'CAND_ID',
                            node_names = unique(df$CMTE_ID))
smry <- summary(cascades) 

microbenchmark(
netinf(cascades, n_edges = 1, lambda = 1), times = 1
)
