ridgeplots <- function(df, x){


g <- ggplot(df, aes(x = danceability, y = as.factor(year), fill = ..x..)) +
    geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
    scale_fill_viridis(option = "C") +
    labs(
        title = "Danceability Trends Over Time",
        x = "Danceability (0-1 scale)",
        y = "Year"
    ) +
    theme_ipsum() +
    theme(
        legend.position = "none",
        axis.text.y = element_text(size = 9)  # Slightly larger year labels
    )
g
}