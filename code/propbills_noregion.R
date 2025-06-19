propbills_noregion <- function(df){
    df %>%
        filter(!is.na(year), !is.na(self_made)) %>%
        mutate(
            self_made = ifelse(self_made, "Self-made", "Inherited")
        ) %>%
        count(year, self_made) %>%
        ggplot(aes(x = year, y = n, fill = self_made)) +
        geom_area(position = "fill") +
        scale_y_continuous(labels = scales::percent_format()) +
        scale_fill_brewer(palette = "Set2") +
        labs(title = "",
            subtitle = glue::glue("Non-US Countries"),
            x = "Year",
            y = "Proportion",
            fill = "Wealth Origin"
        ) +
        theme_minimal()
}