library(boxr) 
library(tidyverse)

# Read VLC_16_catalist.csv from box
catalist_data = box_read_csv(file_id = '302341085102') %>% 
    tbl_df() %>%
    select(Actor_ID, )

