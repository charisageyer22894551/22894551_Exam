live_timetrend_comp <- function(df_artist, df_compare, artist_name, compare_name, variable) {

    combined_data <- bind_rows(
        df_artist %>% mutate(source = artist_name),
        df_compare %>% mutate(source = compare_name)
    )

g <- ggplot(combined_data, aes(x = year, y = .data[[variable]], color = source)) +
    geom_point(alpha = 0.5, size = 1) +
    geom_smooth(
        aes(fill = source),
        method = "loess",
        se = TRUE,
        alpha = 0.2
    ) +
    scale_color_manual(values = c("red", "steelblue")) +
    scale_fill_manual(values = c("red", "steelblue")) +
    labs(
        title = glue::glue("{artist_name} vs {compare_name}: {variable} Trends"),
        x = "Year",
        y = variable,
        color = "Source",
        fill = "Source"
    ) +
    theme_bw() +
    theme(legend.position = "bottom")
g

}