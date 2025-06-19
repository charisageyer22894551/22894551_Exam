## check out new self-made billionaires in the 1990s

billions_sectors90s  <- function(df){

df_plot <- df %>%
    filter(self_made == "TRUE", decade == "1990") %>%
    mutate(sector = ifelse(grepl("retail|services|consumer|real", industry, ignore.case = TRUE), "Consumer",
            ifelse(grepl("financial|equity|banking|funds|money", industry, ignore.case = TRUE), "Financial",
                   ifelse(grepl("software|tech|technology|computer|internet", industry, ignore.case = TRUE), "Software",
                    "Other")))) %>% # proud of this one :)
    group_by(gdp_country, sector) %>%
    summarise(billionaire_count = n(), .groups = "drop")

# Create the plot
g <- ggplot(df_plot,
       aes(x = gdp_country, y = billionaire_count, color = sector)) +
    geom_point(size = 3, alpha = 0.7) +
    #geom_smooth(method = "lm", se = FALSE, linewidth = 1.5) +  # Linear trendlines
    scale_x_continuous(labels = scales::dollar_format(scale = 1e-12, suffix = "T"),
        trans = "log10") +
    scale_color_manual(values = c("Software" = "#0072B2", "Consumer" = "#D55E00", "Financial" = "#00FF00")) +
    labs(
        title = "Self-Made Billionaires in the 1990s by Country GDP",
        subtitle = "Software vs. Consumer vs. Financial Services Industries",
        x = "Country GDP (log scale, USD)",
        y = "Number of Billionaires",
        color = "Industry"
    ) +
    theme_minimal() +
    theme(
        legend.position = "top",
        plot.title = element_text(face = "bold")
    )

g
}