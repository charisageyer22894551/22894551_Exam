# top 3 by increase
get_top3_increasing_names <- function(df, gender_filter = "F") {
    df %>%
        # Filter gender and calculate decades
        filter(Gender == gender_filter) %>%
        mutate(decade = floor(year / 10) * 10) %>%

        # Calculate totals by name and decade
        group_by(name, decade) %>%
        summarise(total = sum(Count), .groups = "drop") %>%

        # Calculate percentage change
        arrange(name, decade) %>%
        group_by(name) %>%
        mutate(
            perc_change = (total - lag(total)) / lag(total) * 100
        ) %>%
        ungroup() %>%

        # Get top 3 per decade
        group_by(decade) %>%
        slice_max(perc_change, n = 3) %>%
        mutate(rank = paste0("#", row_number())) %>%
        ungroup() %>%

        # make the table

        # Format as comma-separated table
        select(decade, rank, name, perc_change) %>%
        pivot_wider(
            names_from = rank,
            values_from = c(name, perc_change),
            names_glue = "{rank}_{.value}"
        ) %>%
        mutate(Top_3_Names = paste(`#1_name`, `#2_name`, `#3_name`, sep = ", "),
            Top_3_Increase = paste(
                round(`#1_perc_change`, 1), "%",
                round(`#2_perc_change`, 1), "%",
                round(`#3_perc_change`, 1), "%",
                sep = ", "           )
        ) %>%
        select(
            Decade = decade,
            `Top 3 Names` = Top_3_Names,
            `Percentage Increase` = Top_3_Increase
        )
}