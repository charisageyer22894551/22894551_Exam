
get_top_names_in_billboard <- function(Billboard_MF, Baby_Names) {
    # Get top 10 songs per decade
    top_songs <- Billboard_MF %>%
        mutate(decade = 10 * floor(year / 10)) %>%
        group_by(decade, song, artist) %>%
        summarise(weeks_at_1 = sum(peak == 1), .groups = "drop") %>%
        group_by(decade) %>%
        slice_max(weeks_at_1, n = 10) %>%
        ungroup()

    # Extract all names from both song titles and artist names
    all_names <- Baby_Names %>% pull(name) %>% unique()

    # Create regex pattern for name matching
    name_pattern <- paste0("\\b(", paste(all_names, collapse = "|"), ")\\b")

    # Find matches in song titles or artist names
    top_songs %>%
        mutate(
            matched_names = map2_chr(
                song, artist,
                ~ str_extract_all(
                    paste(.x, .y),
                    regex(name_pattern, ignore_case = TRUE)
                ) %>%
                    unlist() %>%
                    unique() %>%
                    paste(collapse = ", ")
            ) %>%
                filter(matched_names != "") %>%
                select(decade, song, artist, matched_names) %>%
                arrange(decade)
        )
            }