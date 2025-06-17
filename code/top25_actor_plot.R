# create function for top 25 actors and characters


top25_actor_plot <- function(df, xaxis_size = 10, xaxis_rows = 2, gender_filter, name_col) {

    summary_df <- df %>%
        left_join(HBO_titles, by = "id") %>%
        filter(!is.na(Gender), !is.na(release_year)) %>%
        mutate(decade = floor(release_year / 10) * 10) %>%
        group_by(decade, Gender, name_col) %>%
        summarise(
            Total = n(),  # or sum(tmdb_score) if you want score-weighted
            .groups = "drop"
        )

    # Filter to selected gender
    top_25 <- df %>%
        filter(Gender == gender_filter)

    # Get top 25 names per decade
    Top25names_decade <- top_25 %>%
        group_by(decade, name_col) %>%
        summarise(Total = sum(Total), .groups = "drop") %>%
        group_by(decade) %>%
        slice_max(order_by = Total, n = 25) %>%
        ungroup()

    # Create name order
    name_order <- Top25names_decade %>%
        group_by(name) %>%
        summarise(Total = sum(Total), .groups = "drop") %>%
        arrange(desc(Total)) %>%
        pull(name)

    # Apply ordering
    Top25_plot <- Top25names_decade %>%
        plot_orderset("name", name_order) %>%
        plot_orderset("decade", sort(unique(Top25names_decade$decade)))

    gender_label <- ifelse(gender_filter == "F", "Female name", "Male name")

    # Create plot
    ggplot(Top25_plot, aes(x = name, y = factor(decade), size = Total, colour = name)) +
        geom_point(alpha = 0.7) +
        scale_size_continuous(range = c(3, 12)) +
        coord_flip() +
        theme_bw() +
        labs(
            title = glue::glue("Top 25 {gender_label} Names per Decade (HBO)"),
            subtitle = "Bubble size = number of appearances per decade",
            x = "Actor Name", y = "Decade", size = "Total Appearances"
        ) +
        theme(
            plot.title = element_text(size = 14),
            plot.subtitle = element_text(size = 12),
            axis.text.x = element_text(size = xaxis_size, angle = 90),
            axis.text.y = element_text(size = 8),
            legend.position = "none"
        )
}