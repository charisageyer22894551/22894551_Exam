ggridgesbillions <- function(df){

    # use same filtering as before - can probs make this into a function!

    df_plot <- df %>%
        filter(self_made, decade == 1990) %>% # use only "self_made" bc it is a logical i created to it implicitly will only keep the ones that are
                mutate(sector = ifelse(grepl("retail|services|consumer|real", industry, ignore.case = TRUE), "Consumer",
                               ifelse(grepl("financial|equity|banking|funds|money", industry, ignore.case = TRUE), "Financial",
                                      ifelse(grepl("software|tech|technology|computer|internet", industry, ignore.case = TRUE), "Software",
                                             NA_character_)))) %>% # proud of this one :)
        filter(!is.na(sector)) %>% # filter out the NA i just created
        mutate(gdp_country = gdp_country / 1e12) %>%  # make bit more readable
        group_by(gdp_country, sector) %>%
        summarise(billionaire_count = n(), .groups = "drop")

    # for later
    labels_df <- df_plot %>%
        group_by(sector) %>%
        summarise(total = sum(billionaire_count), label_x = 4.5, .groups = "drop") # Fixed x-position just outside the plot range


    #make the ggridges plot
    # Create plot

    g <- ggplot(df_plot, aes(x = gdp_country, y = sector, height = billionaire_count, fill = sector)) +
        geom_density_ridges(stat = "identity", scale = 0.9, alpha = 0.7, bandwidth = 0.5  ) +  # Adjust for smoother/rougher distributions

        scale_x_continuous(limits = c(0, 5),  # Focus on 0-5T GDP only
            breaks = seq(0, 5, by = 1), labels = scales::dollar_format(suffix = "T"), expand = c(0, 0)) +
        geom_label(data = labels_df, aes(x = label_x, y = sector, label = paste0("Total: ", total)), inherit.aes = FALSE, vjust = 1, size = 3.5,
            label.size = 0.2, fill = "white", colour = "black") +
        scale_fill_manual(values = c("Consumer" = "#E69F00", "Financial" = "#56B4E9", "Software" = "#009E73")) +

        labs(
            title = "Self-Made Billionaires by GDP and Sector (1990s)",
            subtitle = "Distribution across country GDP levels",
            x = "Country GDP (in trillions USD)",
            y = "Industry Sector",
            caption = "Data: Forbes Billionaires 1990-1999") +
        theme_ridges() +
        theme(
            legend.position = "none",
            plot.title = element_text(face = "bold", size = 14),
            axis.title = element_text(size = 12)
        )
    g

    }
