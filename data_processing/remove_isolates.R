library(tidyverse)

# Remove isolate donors and candidates iteratively
remove_isolates <- function(dat, threshold) {
    n_row_diff <- 1
    while(n_row_diff > 0) {
        n_row_before <- nrow(dat)
        recip_smry <- group_by(dat, Recip_ID) %>%
            summarize(n_donors = length(unique(Donor_ID)))
        donor_smry <- group_by(dat, Donor_ID) %>%
            summarize(n_recips = length(unique(Recip_ID)))
        dat <- left_join(dat, recip_smry, by=c('Recip_ID')) %>%
            left_join(donor_smry, by=c('Donor_ID')) %>%
            filter(n_donors > threshold, 
                   n_recips > threshold) %>%
            select(-n_donors, -n_recips)
        n_row_diff <- n_row_before - nrow(dat)
        cat(paste0('Removed ', n_row_diff, ' rows.\n'))
    }
    return(dat)
}
