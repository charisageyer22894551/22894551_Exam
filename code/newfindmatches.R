
findmatches_and_matchrate <- function(df, gender_filter = "F", top_n = 25) {

    # Step 1: Get top increasing baby names per decade
    top_babynames_decade <- df %>%
        filter(Gender == gender_filter) %>%
        mutate(decade = floor( year / 10) * 10) %>%
        group_by(decade, name) %>%
        summarise(Total = sum(Count, na.rm = TRUE), .groups = "drop") %>%
        arrange(name, decade) %>%
        group_by(name) %>%
        mutate(perc_change = (Total - lag(Total)) / lag(Total) * 100) %>%
        ungroup() %>%
        group_by(decade) %>%
        slice_max(order_by = perc_change, n = top_n, with_ties = FALSE) %>%
        ungroup() %>%
        mutate(name_clean = str_to_lower(name) %>% str_remove_all("[^a-z]")) %>%
        distinct(decade, name_clean)

    # Step 2: Prepare HBO actor names
    HBO_actors_clean <- HBO_MF %>%
        mutate(decade = floor(year / 10) * 10,
               first_name = str_to_lower(word(name, 1)) %>% str_remove_all("[^a-z]")) %>%
        distinct(decade, first_name)

    # Step 3: Prepare HBO character names
    HBO_characters_clean <- HBO_characters_MF %>%
        mutate(decade = floor(year/ 10) * 10,
               first_name = str_to_lower(word(name, 1)) %>% str_remove_all("[^a-z]")) %>%
        distinct(decade, first_name)

    # Step 4: Combine both sources and join
    all_matches <- bind_rows(
        HBO_actors_clean,
        HBO_characters_clean
    ) %>%
        distinct(decade, first_name)

    # Step 5: Compare top baby names to all_matches
    match_summary <- top_babynames_decade %>%
        mutate(match_key = paste(decade, name_clean)) %>%
        mutate(is_matched = match_key %in% paste(all_matches$decade, all_matches$first_name))

    # Step 6: Calculate overall match %
    overall_match_percentage <- mean(match_summary$is_matched) * 100

    # Step 7: Return result
    list(
        matched_names = match_summary %>% filter(is_matched),
        total_names = nrow(match_summary),
        match_count = sum(match_summary$is_matched),
        match_rate = overall_match_percentage,
        message = glue("Overall, {round(overall_match_percentage, 1)}% of the top {top_n} baby names per decade match a popular HBO actor or character name in the same decade.")
    )
}