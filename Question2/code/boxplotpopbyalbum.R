# boxplot for popularity

boxplotpopbyalbum <- function(df){
q <- df %>%
    ggplot(aes(x = fct_reorder(album, popularity, .fun = median),
               y = popularity,
               fill = album)) +
    geom_boxplot(outlier.size = 1, alpha = 0.7, show.legend = FALSE) +
    labs(
        title = "Popularity by Album",
        x = NULL,
        y = "Popularity"
    ) +
    theme_minimal(base_size = 12) +
    theme(
        axis.text.x = element_text(angle = 90, hjust = 1, size = 8),
        panel.grid.major.x = element_blank(),
        axis.text = element_text(colour = "black"),
        axis.title = element_text(colour = "black"),
        plot.title = element_text(colour = "black", hjust = 0.5)  # Combined into one
    )
q
}