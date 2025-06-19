top25_increaseplot <- function(df, xaxis_size = 10, xaxis_rows = 2, gender_filter = "F") {

    # Step 1: Get totals by decade
    Top25names_decade <- Top25_Names_by_Decade(df, gender_filter = gender_filter)

    # Step 2: Calculate % change per name per decade
    Top25_with_change <- Top25names_decade %>%
        arrange(name, decade) %>%
        group_by(name) %>%
        mutate(perc_change = (Total - lag(Total)) / lag(Total) * 100) %>%
        ungroup()

    # Step 3: Flag top 3 bubbles per decade
    Top25_with_change <- Top25_with_change %>%
        group_by(decade) %>%
        mutate(bubble_rank = rank(-perc_change)) %>%
        ungroup() %>%
        mutate(highlight_top3 = ifelse(bubble_rank <= 3, "Top 3", NA),   # only Top 3 get labelled in legend
            colour_group = name)                            # keep name-based colours

    # Step 4: Factor reorder names for clean plotting
    name_order <- Top25_with_change %>%
        group_by(name) %>%
        summarise(Total = sum(Total, na.rm = TRUE), .groups = "drop") %>%
        arrange(desc(Total)) %>%
        pull(name)

    Top25_plot <- Top25_with_change %>%
        plot_orderset("name", name_order) %>%
        plot_orderset("decade", sort(unique(Top25_with_change$decade)))

    gender_label <- ifelse(gender_filter == "F", "Girl", "Boy")

    # Step 5: Final ggplot with all the layers
    g <- ggplot(Top25_plot, aes(x = name, y = factor(decade), size = perc_change)) +

        # All name-coloured bubbles (no legend)
        geom_point(aes(colour = colour_group), alpha = 0.5, show.legend = FALSE) +

        # Highlighted Top 3 bubbles (with legend)
        geom_point(data = filter(Top25_plot, !is.na(highlight_top3)),
            aes(colour = highlight_top3), size = 6, alpha = 0.9, show.legend = TRUE) +

        #label each of the top 3 the name of the bubble
        geom_text(data = filter(Top25_plot, !is.na(highlight_top3)),
            aes(label = name), colour = "black", size = 3, vjust = 0, check_overlap = TRUE) +

        scale_size_continuous(range = c(1, 10), name = "% Change") +
        scale_colour_manual(values = c("Top 3" = "yellow")) +
        coord_flip() +
        theme_bw() +
        labs(title = glue::glue("Top 25 {gender_label} Names – % Increase per Decade"),
            subtitle = "Red bubbles show Top 3 decade-on-decade % increases",
            x = "Name", y = "Decade") +

        theme(
            plot.title = element_text(size = 14),
            plot.subtitle = element_text(size = 12),
            axis.text.x = element_text(size = xaxis_size, angle = 45, hjust = 1),
            axis.text.y = element_blank(), # rather leave out becaus it looks so messy
            legend.position = "right",
            legend.title = element_blank()
        )

    return(g)
}

# (size = xaxis_size, angle = 45, hjust = 1),