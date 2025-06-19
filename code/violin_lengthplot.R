violin_lengthplot <-function(df){

    df_plot <- df %>%
                filter(type == "MOVIE") %>%
                filter(!is.na(runtime), !is.na(imdb_score)) %>%
                group_by(genres) %>%
                filter(n() >= 2) %>%  # Only show genres with ≥2 movies!
                ungroup()

        g <- df_plot %>%
                ggplot(aes(x = runtime, y = reorder(genres, runtime), fill = genres)) + # each band has own colour
                geom_violin(alpha = 0.7, show.legend = FALSE) +
                #geom_jitter(aes(size = imdb_score), width = 0.01, alpha = 0.2, height = 0.01 ) + # Map size = IMDB score
                coord_flip() +
                scale_size_continuous(range = c(1, 5)) +
                scale_fill_viridis_d() +
                theme_bw() +
                theme(legend.position = "right") +
                labs(x = "Movie Length (minutes)",
                     y = "Genre",
                     size = "IMDB Score",
                     title = "Movie Length by Genre" ) +
                     # subtitle = "Bubbles show IMDB score") +
                theme(legend.position = "none",
                      axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
                      panel.grid.major.y = element_line(color = "grey90"))

            return(g)
}

