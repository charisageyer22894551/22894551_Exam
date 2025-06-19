top10countries <- function(df){

     df %>%
        filter(type == "Movie") %>%
        count(production_countries, name = "total_movies") %>%
        arrange(desc(total_movies)) %>%
        head(10)  # Select top 10
}
