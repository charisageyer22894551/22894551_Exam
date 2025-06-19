movie_scatterplot <- function(df) {
    library(ggsci) #sommer a lekker colourblind firendly one

    df_plot <- df %>%  filter(type == "MOVIE", !is.na(imdb_score), !is.na(runtime))   # filtering step

   g<-  ggplot(df_plot,
           aes(x = runtime, y = imdb_score, color = genres)) +
        geom_point(size = 3, alpha = 0.7) +
       geom_hline(
           yintercept = 7.5,           # Reference line at 7.5
           color = "black",
           linetype = "dashed",
           linewidth = 0.5,
           alpha = 0.5
       ) +
       scale_color_viridis_d() +
        labs(
            x = "Runtime (minutes)",
            y = "IMDB Rating",
            color = "Genre",
            title = "IMDB Ratings vs Movie Length"
        ) +
        theme_minimal() +
        theme(legend.position = "right")
    g
}

