library(boxr) 
library(tidyverse)
library(yaml)

# THis is still a TODO!

config = yaml.load_file('0_config.yml')
LOCAL_DATA = config$LOCAL_DATA

if(!is.null(LOCAL_DATA)) {
    random_reply = read_csv(paste0(LOCAL_DATA, 
                                   'catalist_random_donors_ideology.csv')) 
    active_reply = read_csv(paste0(LOCAL_DATA, 
                                   'catalist_active_donor_ideology.csv')) 
}

# Read VLC_16_catalist.csv from box
catalist_data = box_read_csv(file_id = '302341085102') %>% 
    tbl_df() %>%
    select(Actor_ID, )

