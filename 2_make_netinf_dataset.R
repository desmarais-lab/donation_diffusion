library(tidyverse)
library(boxr)
library(yaml)

source('remove_isolates.R')

config = yaml.load_file('0_config.yml')

# Read 'EL_16.csv' from box
df = box_read_csv(file_id = '302149820582', fread = TRUE)
date_low = as.Date('2015-01-01')
date_high = as.Date('2017-01-01')

df = select(df, Donor_ID, Recip_ID, Amt, Tran_Tp, Recip_Tp, Date, Donor_Tp) %>%
    # Remove rows with missing data
    na.omit() %>%
    mutate(Date = as.Date(Date, '%m/%d/%Y')) %>%
    # Remove transactions that are not considered donations
    # Remove transactions with negative and 0 amount (refunds)
    # Rows with transaction dates outside the period
    # With non-candidate recipients
    filter(!is.element(Tran_Tp, c('19', '24A', '24C', '24E', '24F', 
                                  '24N', '29')),
           Amt > 0, Date >= date_low, Date < date_high, Recip_Tp == 'CAND') %>%
    group_by(Donor_ID, Recip_ID) %>%
    # Only keep the first donation (in time) for each donor - recipient dyad
    arrange(Date) %>%
    filter(row_number() == 1)

# Subset the data. Remove:
# - Recipients with less than 8 unique donors
# - Donors that give to less than 8 candidates
# - Iteratively remove them untill all (8-)isolates are gone:
df = remove_isolates(df, threshold = config$ISOLATE_THRESHOLD)
df$integer_date = as.integer(df$Date)

## Write files to box in dir 'Strategic_Donors/final_paper_data/'
fname = paste0('data_for_netinf_threshold_', config$ISOLATE_THRESHOLD, '.csv')
ref = box_write(df, filename = fname, write_fun = write_csv, 
                dir_id = '50855821402')
## Store file reference (file_id) in config for downstream scripts
config[[fname]] = ref$id

## Write config
write_yaml(config, file = '0_config.yml')
