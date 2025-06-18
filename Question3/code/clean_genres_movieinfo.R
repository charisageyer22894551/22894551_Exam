# clean genres to match the other dataframe!

clean_listed_in <- function(df) {
    df %>%
        filter(!Listed_In %in% c("", "NA", "[]")) %>% #same as before
        mutate(Listed_In = Listed_In %>%
                               tolower() %>% # make lowercase
                   # try to match genre format
                      gsub("children & family movies", "family", .) %>%
                gsub("sci-fi & fantasy", "fantasy", .) %>%
                gsub("music & musicals", "music", .) %>%
                gsub("international movies", "international", .) %>%
                gsub("independent movies", "independent", .) %>%
                gsub("romantic movies", "romance", .) %>%
                gsub("horror movies", "horror", .) %>%
                   gsub("thrillers", "thriller", .) %>%
                   gsub("dramas", "drama", .) %>%
                   gsub("documentaries", "documentary", .) %>%
                   gsub("action & adventure", "action", .) %>%
                   gsub("anime features", "anime", .) %>%
                   gsub("comedies", "comedy", .) %>%
                   gsub("lgbtq movies", "lgbtq", .) %>%
                                   # Format cleanup (matches genre cleaning)
                gsub("\\[|\\]|'|\"", "", .) %>%
                gsub("\\s*,\\s*", ", ", .)
        )
}