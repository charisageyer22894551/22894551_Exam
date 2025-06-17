# Spearman correlation plot

Spearman_Name_Persistence <- function(df, gender_filter = "F", top_n = 25, lag_n = 3) {
    # Prep top N names per year for chosen gender
    top_names_by_year <- df %>%
        filter(Gender == gender_filter) %>%
        group_by(Year, Name) %>%
        summarise(Total = sum(Count), .groups = "drop") %>%
        group_by(Year) %>%
        slice_max(order_by = Total, n = top_n) %>%
        mutate(rank = rank(-Total)) %>%
        ungroup()

    # Loop over years and compute correlation with next 3 years
    year_range <- unique(top_names_by_year$year)

    results <- map_dfr(year_range, function(base_year) {
        base <- top_names_by_year %>% filter(year == base_year)

        future_years <- (base_year + 1):(base_year + lag_n)

        map_dfr(future_years, function(future_year) {
            future <- top_names_by_year %>% filter(year == future_year)

            joined <- inner_join(base, future, by = "Name", suffix = c("_base", "_future"))

            if (nrow(joined) >= 5) {
                tibble(
                    base_year = base_year,
                    compare_year = future_year,
                    spearman_rho = cor(joined$rank_base, joined$rank_future, method = "spearman")
                )
            } else {
                tibble(
                    base_year = base_year,
                    compare_year = future_year,
                    spearman_rho = NA_real_
                )
            }
        })
    })

    results


   g <- ggplot(spearman_girls, aes(x = base_year, y = spearman_rho)) +
        geom_line(colour = "deeppink") +
        geom_point() +
        theme_minimal() +
        labs(
            title = "Naming Persistence (Girls)",
            subtitle = "Spearman Rank Correlation of Top 25 Names and Following Years",
            x = "Base Year", y = "Spearman ρ"
        )
   g
}