violin_lengthplot2 <- function(df) {
    library(dplyr)
    library(ggplot2)

    df_plot <- df %>%
        filter(type == "MOVIES") %>%  # Changed from "MOVIES" to "MOVIE"
        group_by(genres) %>%
        filter(n() >= 2) %>%
        ungroup()

    ggplot(df_plot, aes(x = runtime, y = reorder(genres, runtime), fill = genres)) +
        geom_violin(alpha = 0.7, show.legend = FALSE) +
        geom_jitter(aes(size = imdb_score), width = 0.1, alpha = 0.5, height = 0.1) +
        scale_size_continuous(range = c(1, 5)) +
        labs(x = "Movie Length (minutes)",
             y = "Genre",
             size = "IMDB Score",
             title = "Movie Length by Genre") +
        theme_bw() +
        theme(legend.position = "bottom")
}