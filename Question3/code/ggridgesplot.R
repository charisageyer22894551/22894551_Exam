ggridgesplot <- function(df){

    library(ggridges)

    # first calucalte the mean imdb score for the genre!

    mean_genre <- df %>%
        filter(type == "MOVIE") %>%
        group_by(genres) %>%
        summarise(mean_score = mean(imdb_score, na.rm = TRUE)) %>%
        ungroup() %>%
        arrange(mean_score)

    # create the ggplot
        g <- df %>%
        ggplot(aes(x = imdb_score, y = genres, fill = genres)) +
        geom_density_ridges(alpha = 0.7) +
        labs(title = "Average IMDB scores per genre",
             subtitle = "Movies only", x = "IMDB Score", y = "Genre") +
        theme_minimal() +
        theme(legend.position = "none") +
        geom_vline(
            xintercept = mean(Titles$imdb_score, na.rm = T),
            linetype = "dotted",
            color = "steelblue"
        ) +
        geom_text(data = mean_genre,
                  aes(x = mean_score, y = genres, label = round(mean_score, 1)), # move the score just a little to the left # okay no it looks worse, keep as is!
                  nudge_y = 0.25, size = 3) +
        geom_point(data = mean_genre, aes(x = mean_score, y = genres), color = "black", size = 0.5) # mena point a bit smaller


    g
}