library(NetworkInference)
library(xtable)
library(tidyverse)

result_dir <- '../data/results/'
source('plot_theme.R')

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Explore distributions of donors wrt their number of unique donations conditional
# on membership in inference data, and being tied into the network
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Load the 2016 diffusion network
load(paste0(result_dir, '2016_output.RData'))

year <- 2016
infile <- paste0('../data/EL_', substr(as.character(year), 3, 4), '.csv')
date_low <- as.Date(paste0(as.character(year - 1), '-01-01'))
date_high <- as.Date(paste0(as.character(year + 1), '-01-01'))
## This mirrors the first pre processing step in network_inference.R
df <- read_csv(infile) %>%
    select(Donor_ID, Recip_ID, Amt, Tran_Tp, Recip_Tp, Date, Donor_Tp) %>%
    na.omit() %>%
    mutate(Date = as.Date(Date, '%m/%d/%Y')) %>%
    filter(!is.element(Tran_Tp, c('19', '24A', '24C', '24E', '24F', 
                                  '24N', '29')),
           Amt > 0, Date >= date_low, Date < date_high, Recip_Tp == 'CAND') %>%
    group_by(Donor_ID, Recip_ID) %>%
    arrange(Date) %>%
    filter(row_number() == 1)
    
donors <- group_by(df, Donor_ID) %>%
    summarize(Donor_Tp = Donor_Tp[1],
              n_records = n())

# Indicators for different donor groups (in the data used for inference and in the network (i.e. being tied to other donors))
din <- unique(c(output$network$origin_node, output$network$destination_node))
diif <- unique(output$data$Donor_ID)
donors$in_inference_data <- is.element(donors$Donor_ID, diif)
donors$in_network <- is.element(donors$Donor_ID, din)

pdat <- filter(donors, in_network)
summary(pdat$n_records)
ggplot(pdat) +
    geom_histogram(aes(n_records), color = "white", bins = 30) + 
    #scale_x_log10() +
    plot_theme

pdat <- filter(donors, in_inference_data, !in_network)
summary(pdat$n_records)
ggplot(pdat) +
    geom_histogram(aes(n_records), color = "white", bins = 30) + 
    #scale_x_log10() +
    plot_theme

pdat <- filter(donors, !in_inference_data)
summary(pdat$n_records)
ggplot(pdat) +
    geom_histogram(aes(n_records), color = "white", bins = 30) + 
    #scale_x_log10() +
    plot_theme

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Take a stratified sample
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Load the donor information file to get all info required for catalist matching
# and join to donation count df
donor_file <- '../data/Indivs_16.csv'
donor_info <- read_csv(donor_file)
population <- inner_join(donors, donor_info, by = c("Donor_ID" = "Actor_ID")) %>%
    filter(!is.na(Name))

set.seed(67814)
samp1 <- filter(population, n_records == 1) %>%
    mutate(s = sample(1:nrow(.), nrow(.), replace=FALSE)) %>%
    filter(s <= 1000) %>%
    mutate(stratum = "1")
samp2_4 <- filter(population, n_records > 1, n_records < 5) %>%
    mutate(s = sample(1:nrow(.), nrow(.), replace=FALSE)) %>%
    filter(s <= 1000) %>%
    mutate(stratum = "2_4")

out <- rbind(samp1, samp2_4) %>%
    select(Name, n_records, stratum, Donor_ID)
write_csv(out, path = '../data/stratified_donor_sample.csv')
