# function to clean the genre and production country columns specifically for TITLES

clean_genres_titles <- function(df) {
         df %>%
        filter(!genres %in% c("", "[]", "NA")) %>%  # Remove bad values first
        mutate(genres = genres %>%

                gsub("\\[|\\]|'|\"", "", .) %>%  # Remove brackets and quotes
                gsub("\\s*,\\s*", ", ", .) %>%   # Standardise the comma spacing
                gsub("documentation", "documentary", .)) %>% #  Just make it more consistent
            separate_rows(genres, sep = ",\\s*")  #creates each of the genres its own row


}
