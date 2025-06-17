# create function for plotting the point plots
top25_plot <- function(df, xaxis_size, xaxis_rows, gender_filter){

    # create name order

   Top25names_decade <- Top25_Names_by_Decade(df, gender_filter = gender_filter)

name_order <- Top25names_decade %>%
    group_by(Name) %>%
    summarise(Total = sum(Total)) %>%
    arrange(desc(Total)) %>%
    pull(Name)

Top25_plot <- Top25names_decade %>%
    plot_orderset("Name", name_order) %>%
    plot_orderset("decade", sort(unique(Top25names_decade$decade)))

# Gender label for title/subtitle
gender_label <- ifelse(gender_filter == "F", "Girl", "Boy")

# create the ggplot
g <- ggplot(Top25_plot, aes(x = Name, y = factor(decade), size = Total, colour = Name)) +
    geom_point(alpha = 0.7) +
    scale_size_continuous(range = c(3, 12)) +
    coord_flip() +
    theme_bw() +
    labs(title = glue::glue("Top 25 {gender_label} Names per Decade (US)"),
         subtitle = "Bubble size shows total babies given that name per decade",
         x = "Name", y = "Decade", size = "Total Count") +
    theme(
        plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 12),
        axis.text.x = element_text(size = 12, angle = 90),
        axis.text.y = element_text(size = 8),
        legend.position = "none"
    )
g
}

