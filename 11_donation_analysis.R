# This script generates analyses that are based directly on the raw donation
# data. And some descriptives based on the inferred diffusion network
#
# - Figure of normalized donation rank by donor ideology and recipient party
# - Figure of proportion of donations to incumbent/non-incument by donor 
#   ideology and recipient party

library(boxr)
library(tidyverse)
#devtools::install_github('flinder/flindR')
library(flindR) # For plotting theme
library(yaml)
library(xtable)

config = yaml.load_file('0_config.yml')
LOCAL_DATA = config$LOCAL_DATA
P_VALUE = config$P_VALUE
pe = plot_elements()
TABLE_DIR = 'paper/tables/'
FIG_DIR = 'paper/figures/'

if(!is.null(LOCAL_DATA)) {
    entities = read_csv(paste0(LOCAL_DATA, 'VLC_16_full.csv'))
    donations = read_csv(paste0(LOCAL_DATA, 'data_for_netinf_threshold_8.csv')) %>% 
        filter(Donor_Tp == 'IND')
} else {
    box_auth()
    # Load 'Strategic_Donors/VLC_16_full.csv' from box
    entities = box_read(file_id = '308095557675', read_fun = read_csv)
    # Load the cascade data 'data_for_netinf_threshold_8.R' from box
    donations = box_read_csv(file_id = '302844881600') %>% 
        filter(Donor_Tp == 'IND')
    # Load netinf_network_threshold_8_bugfix.RData (object named 'network')
    box_load(file_id = '336545819172')
}

donors = entities %>%
    filter(Ent_Typ == 'IND', ideology != "") %>%
    select(Actor_ID, ideology)

candidates = entities %>%
    filter(Ent_Typ == 'CAND') %>%
    select(Actor_ID, Party_PAC_Type, Incum)

   

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

## Join with the candidate data for plotting
donations = left_join(donations, candidates, by = c('Recip_ID' = 'Actor_ID')) %>%
    filter(is.element(Party_PAC_Type, c('D', 'R')))

## Convert the ideology bins to numeric values by assigning values uniformly 
## random between the end points of the bin
splt = strsplit(donations$ideology, '–')
a = sapply(splt, function(x) runif(1, as.numeric(x)[1], as.numeric(x)[2]))
donations$ideology = a

## Separate for incumbents and non-incumbents
donations$incumbent = ifelse(donations$Incum == "I", "Incumbent", "Non-Incumbent")
donations = filter(donations, !is.na(incumbent))
donations$ideology = donations$ideology - 50

ggplot(donations, aes(x = ideology, y = normalized_rank, 
                      color = Party_PAC_Type, linetype = Party_PAC_Type)) +
    geom_jitter(alpha = 0.1, size = 0.1) +
    geom_smooth() +
    pe$theme + xlab('Donor Ideology') + ylab('Normalized Donation Rank') +
    scale_color_manual(values = c('#4286f4', '#f44141'), 
                       name = "Recipient\nParty",
                       labels = c("Democrat", 'Republican')) +
    scale_linetype(name = "Recipient\nParty",
                   labels = c("Democrat", "Republican")) +
    facet_wrap(~incumbent)
ggsave(paste0(FIG_DIR, 'donation_rank.png'), width = pe$p_width, 
       height = 0.7 * pe$p_width)


# Donor ideology and donations to incumbents 
donations$incumbent_b = ifelse(donations$incumbent == "Incumbent", 1, 0)
ggplot(donations, aes(x = ideology, y = incumbent_b, color = Party_PAC_Type,
                      linetype = Party_PAC_Type)) +
    geom_smooth() +
    xlab('Donor Ideology') + ylab('Proportion Incumbent') +
    scale_color_manual(values = c('#4286f4', '#f44141'), 
                       name = "Recipient\nParty",
                       labels = c("Democrat", 'Republican')) +
    scale_linetype(name = "Recipient\nParty",
                   labels = c("Democrat", "Republican")) +
    pe$theme + ylim(0,1) 
ggsave(paste0(FIG_DIR, 'donor_ideology_candidate_incumbency.png'), 
       width = pe$p_width, 
       height = 0.7 * pe$p_width)

### Get some descriptives of the donors in the network
ent_type = select(entities, Actor_ID, Ent_Typ)
network = left_join(network, ent_type, by = c("origin_node" = "Actor_ID")) %>%
    rename(origin_entity_type = Ent_Typ) %>%
    left_join(ent_type, by = c('destination_node' = 'Actor_ID')) %>%
    rename(destination_entity_type = Ent_Typ)

tab = group_by(na.omit(network), origin_entity_type, destination_entity_type) %>%
    summarize(Count = n(),
              Proportion = round(n() / nrow(network), 2)) %>%
    rename(Origin = origin_entity_type, 
           Destination = destination_entity_type)
    
tab$Origin[tab$Origin == 'IND'] = "Individual"
tab$Destination[tab$Destination == 'IND'] = "Individual"
print(xtable(tab, caption = "Tie types in the inferred diffusion network.", 
             label = 'tab:tie_types'),
      file = paste0(TABLE_DIR, 'tie_types.tex'), include.rownames = FALSE)
