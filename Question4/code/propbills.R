# proportion billionaires

propbills <- function(df, location_region){
    df %>%
        filter(!is.na(year), !is.na(self_made), location_region == location_region) %>%
        mutate(
            self_made = ifelse(self_made, "Self-made", "Inherited")
        ) %>%
        count(year, self_made) %>%
        # make the ggplot
        ggplot(aes(x = year, y = n, fill = self_made)) +
        geom_area(position = "fill") +
        scale_y_continuous(labels = scales::percent_format()) +
        scale_fill_brewer(palette = "Set2") +
        labs(
            title = glue::glue("Proportion of Self-Made vs Inherited Billionaires Over Time"),
            subtitle = glue::glue("{location_region}"),
            x = "Year",
            y = "Proportion",
            fill = "Wealth Origin"
        ) +
        theme_minimal()
}