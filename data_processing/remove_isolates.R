# Remove isolate donors and candidates iteratively
remove_isolates <- function(df, isolate_threshold) {
    n_row_diff <- 1
    while(n_row_diff > 0) {
        n_row_before <- nrow(df)
        recip_smry <- group_by(df, Recip_ID) %>%
            summarize(n_donors = length(unique(Donor_ID)))
        donor_smry <- group_by(df, Donor_ID) %>%
            summarize(n_recips = length(unique(Recip_ID)))
        df <- left_join(df, recip_smry, by=c('Recip_ID')) %>%
            left_join(donor_smry, by=c('Donor_ID')) %>%
            filter(n_donors > isolate_threshold, 
                   n_recips > isolate_threshold) %>%
            select(-n_donors, -n_recips)
        n_row_diff <- n_row_before - nrow(df)
        cat(paste0('Removed ', n_row_diff, ' rows.\n'))
    }
    return(df)
}
