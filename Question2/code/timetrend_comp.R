timetrend_comp <- function(df, artist, variable, min_year = 2000) {

    #filter to minimum year!
    artist_data <- df %>%
        filter(year >= min_year) %>%
        mutate(source = artist)

    spotify_data <- spotify_top100 %>%
        filter(year >= min_year) %>%  # Apply same year filter as the artist
        mutate(source = "Spotify Top 100")

    # Combine data
    combined_data <- bind_rows(artist_data, spotify_data)

    # Create the ggplot
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
            title = glue::glue("{artist} vs Spotify Top 100: {variable} Trends Since {min_year}"),
            x = "Year",
            y = variable,
            color = "Source",
            fill = "Source"
        ) +
        theme_bw() +
        theme(legend.position = "bottom")
    g
}