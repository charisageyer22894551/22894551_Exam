Spearman <- function(df, gender_filter = "F", top_n = 25, lag_n = 3) {


    df_clean <- df %>%
        filter(Gender == gender_filter) %>%
        group_by(year, name) %>%
        summarise(Total = sum(Count), .groups = "drop") %>%
        group_by(year) %>%
        slice_max(order_by = Total, n = top_n) %>%
        mutate(rank = rank(-Total)) %>%
        ungroup()

    year_range <- unique(df_clean$year)

    cor_results <- map_dfr(year_range, function(y) {
        base <- df_clean %>% filter(year == y)
        comparisons <- df_clean %>% filter(year %in% (y + 1):(y + lag_n))

        map_dfr(unique(comparisons$year), function(y_future) {
            future <- comparisons %>% filter(year == y_future)

joined <- inner_join(base, future, by = "name", suffix = c("_base", "_future"))

if (nrow(joined) >= 5) {tibble (base_year = y, compare_year = y_future, spearman_rho = cor(joined$rank_base, joined$rank_future, method = "spearman"))}
else {tibble(base_year = y, compare_year = y_future, spearman_rho = NA_real_ )}
                                                                })
                                                     })

    cor_results

    # Set gender-specific plot colour
    gender_label <- ifelse(gender_filter == "F", "Girls", "Boys")
    line_colour <- ifelse(gender_filter == "F", "deeppink", "steelblue")

    # create the ggplot
    g <- ggplot(cor_results, aes(x = base_year, y = spearman_rho)) +
        geom_line(colour = line_colour, linewidth = 1) +
        geom_point(size = 2, alpha = 0.8) +
        theme_minimal(base_size = 12) +
        labs(
            title = glue::glue("Naming Persistence for {gender_label}"),
            subtitle = glue::glue("Spearman Correlation of Top {top_n} Names vs. Next {lag_n} Years"),
            x = "Base Year", y = "Spearman Rank Correlation (ρ)"
        ) +
        ylim(0, 1) +
        theme(
            plot.title = element_text(size = 14, face = "bold"),
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 10)
        )

   g
}
