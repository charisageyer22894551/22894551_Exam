#function for top 25



Top25_Names_by_Decade <- function(df, gender_filter = "F") {
    df %>%
        distinct() %>%
        filter(Gender == gender_filter) %>%
        mutate(decade = floor(Year / 10) * 10) %>%
        group_by(decade, Name) %>%
        summarise(Total = sum(Count), .groups = "drop") %>%
        group_by(decade) %>%
        slice_max(order_by = Total, n = 25) %>%
        ungroup()
}
