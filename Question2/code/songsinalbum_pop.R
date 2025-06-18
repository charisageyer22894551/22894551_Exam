
songsinalbum_pop<-function(df){
    library(dplyr)
    g<-df %>%  group_by(album) %>%
        filter(n() >= 2) %>%
        ungroup() %>%
        ggplot() +
        aes(x = album, y = popularity, colour = artist, fill = artist) + # each band has own colour
        geom_violin() +
        geom_jitter(aes(size = tempo), width = 0.1, alpha = 0.2) + # Map size = tempo
        coord_flip() +
        theme_bw() +
        theme(legend.position = "right") +
        labs(
            title = "Song Popularity in Album",
            subtitle = "Coldplay and Metallica",
            colour = "Artist",
            fill = "Artist",
            size = "Tempo of Song"
        ) +
        xlab("") +
        ylab("Popularity Score")
    return(g)
}