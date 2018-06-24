library(boxr)
library(tidyverse)
#devtools::install_github('flinder/flindR')
library(flindR) # For plotting theme
pe = plot_elements()

box_auth()

# Load 'Strategic_Donors/2016_data_match/VLC_16_full.csv' from box
entities = box_read_csv(file_id = '255302928759', fread = TRUE) %>% 
    tbl_df() 
donors = entities %>%
    filter(Ent_Typ == 'IND', ideology != "") %>%
    select(Actor_ID, ideology)

candidates = entities %>%
    filter(Ent_Typ == 'CAND') %>%
    select(Actor_ID, Party_PAC_Type, Incum)

# Load the cascade data 'Strategic_Donors/Data/data_for_netinf.R' from box
donations = box_read_csv(file_id = '292888533329') %>% 
    tbl_df() %>%
    filter(Donor_Tp == 'IND')
    

# Get the normalized rank of each donor with catalyst ideology scores in each 
# cascade

## Join ideology to donation data and remove all donations where we don't have
## the ideology of the donor
donations = left_join(donations, donors, by = c('Donor_ID' = 'Actor_ID')) %>% 
    filter(!is.na(ideology))

## Get the rank of each donor in a candidate cascade
donations = arrange(donations, Recip_ID, integer_date) %>%
    group_by(Recip_ID) %>%
    mutate(rank = row_number())

## Get the length of each candidates cascade (normalization constant)
n_donations = group_by(donations, Recip_ID) %>%
    summarize(n_donations = n())
## Join to donation data and calculate normalized rank
donations = left_join(donations, n_donations) %>%
    mutate(normalized_rank = rank / n_donations)

# Join with the candidate data for plotting
donations = left_join(donations, candidates, by = c('Recip_ID' = 'Actor_ID')) %>%
    filter(is.element(Party_PAC_Type, c('D', 'R')))

# Convert the ideology bins to numeric values by assigning values uniformly 
# random between the end points of the bin
splt = strsplit(donations$ideology, '–')
a = sapply(splt, function(x) runif(1, as.numeric(x)[1], as.numeric(x)[2]))
donations$ideology = a

# Separate for incumbents and non-incumbents
donations$incumbent = ifelse(donations$Incum == "I", "Incumbent", "Non-Incumbent")
donations = filter(donations, !is.na(incumbent))

ggplot(donations, aes(x = ideology, y = normalized_rank, 
                      color = Party_PAC_Type, linetype = Party_PAC_Type)) +
    geom_jitter(alpha = 0.1, size = 0.1) +
    #geom_point(alpha = 0.1, size = 0.1) +
    geom_smooth() +
    pe$theme + xlab('Donor Ideology') + ylab('Normalized Donation Rank') +
    scale_color_manual(values = c('#4286f4', '#f44141'), 
                       name = "Recipient\nParty",
                       labels = c("Democrat", 'Republican')) +
    scale_linetype(name = "Recipient\nParty",
                   labels = c("Democrat", "Republican")) +
    facet_wrap(~incumbent)
ggsave('../paper/figures/donation_rank.png', width = pe$p_width, 
       height = 0.7 * pe$p_width)
