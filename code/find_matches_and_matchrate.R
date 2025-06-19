find_matches_and_matchrate <- function(df, gender_filter = "F", top_n = 25) {
    # Step 1: Get top increasing baby names per decade (cleaned)
    top_babynames <- df %>%
        filter(Gender == gender_filter) %>%
        mutate(
            decade = floor(Year / 10) * 10,
            name_clean = str_to_lower(Name) %>% str_remove_all("[^a-z]")
        ) %>%
        group_by(decade, name_clean) %>%
        summarise(Total = sum(Count, na.rm = TRUE), .groups = "drop") %>%
        group_by(decade) %>%
        slice_max(Total, n = top_n, with_ties = FALSE) %>%
        ungroup()

    # Step 2: Prepare HBO names (actors + characters)
    hbo_names <- bind_rows(
        HBO_MF %>% mutate(type = "actor"),
        HBO_characters_MF %>% mutate(type = "character")
    ) %>%
        mutate(
            decade = floor(year / 10) * 10,
            name_clean = str_to_lower(word(name, 1)) %>% str_remove_all("[^a-z]")
        ) %>%
        distinct(decade, name_clean, type)

    # Step 3: Find matches and calculate statistics
    matches <- top_babynames %>%
        left_join(hbo_names, by = c("decade", "name_clean")) %>%
        mutate(
            is_matched = !is.na(type),
            match_type = ifelse(is_matched, type, "no match")
        )

    # Step 4: Calculate match rates
    match_stats <- matches %>%
        group_by(decade) %>%
        summarise(
            total_names = n(),
            matched = sum(is_matched),
            match_rate = matched / total_names * 100,
            .groups = "drop"
        )

    # Step 5: Return comprehensive results
    list(
        matched_names = matches %>% filter(is_matched),
        match_statistics = match_stats,
        overall_match_rate = mean(matches$is_matched) * 100,
        raw_data = matches
    )
}