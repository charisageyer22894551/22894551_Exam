    spearman_analysis <- function(df, top_n = 25, lag_n = 3) {


        df_clean <- df %>%
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
}