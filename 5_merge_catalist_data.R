library(boxr) 
library(tidyverse)
library(yaml)

# THis is still a TODO!

config = yaml.load_file('0_config.yml')
LOCAL_DATA = config$LOCAL_DATA

#if(!is.null(LOCAL_DATA)) {
#    random_reply = read_csv(paste0(LOCAL_DATA, 
#                                   'catalist_random_donors_ideology.csv')) 
#    active_reply = read_csv(paste0(LOCAL_DATA, 
#                                   'catalist_active_donor_ideology.csv')) 
#}
#


# Frido: I couldn't figure out what files exactly are from catlyst, so I merge 
# the 'old' `vlc_16_full.csv` file with the new `vlc_16.csv` to get a full sample
if(!is.null(LOCAL_DATA)) {
    vlc_full = read_csv(paste0(LOCAL_DATA, 'VLC_16_full_old.csv'))
    new_vlc = read_csv(paste0(LOCAL_DATA, 'VLC_16.csv'))
} else {
    box_auth()
    vlc_full = box_read_csv(file_id = '303174011104')
    new_vlc = box_read_csv(file_id = '302126365533')
}

new_vlc_full = left_join(new_vlc, vlc_full) 

if(!is.null(LOCAL_DATA)) {
    write_csv(new_vlc_full, path = paste0(LOCAL_DATA, 'VLC_16_full.csv'))
} else{
    box_write(new_vlc_full, 'VLC_16_full.csv', dir_id = '50855821402')
}
