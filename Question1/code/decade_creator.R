# decade creator

decade_creator <- function(df) {
    df_decades <- df %>%
        mutate(decade = floor(Year / 10) * 10)
    return(df_decades)
}
