top100 <-function(df){

    # Case-insensitive join on song title
    df_top100 <- df %>%
        inner_join(
            billboard_100 %>% distinct(song),  # remove any duplicates in Billboard 100
            by = c("song"),  # look at songs
            # ignore_case = TRUE
        )
}