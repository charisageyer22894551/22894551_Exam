heatmap_billions <- function(df){

    library(tidyverse)
    library(rnaturalearth) # For world map data
    library(rnaturalearthdata) # not sure which one it uses
    library(viridis)

        billionaires_by_country <- df %>%
        group_by(location_citizenship) %>%
        summarise(billionaire_count = n())

        topcountry <- billionaires_by_country %>%
            slice_max(billionaire_count, n = 1)
        topamount <- 903

    # Get world map data
    world <- ne_countries(scale = "medium", returnclass = "sf") %>%
        select(admin, geometry) %>%  # 'admin' contains country names
        left_join(billionaires_by_country, by = c("admin" = "location_citizenship"))

    ggplot(world) +
        geom_sf(aes(fill = billionaire_count), color = "white", size = 0.2) +
        scale_fill_viridis(
            name = "Number of Billionaires",
            option = "magma",
            na.value = "grey90",  # Color for countries with no billionaires
            # trans = "log10",      # Log scale for better visualisation
            breaks = c(1, 10, 100, 1000),
            labels = c(1, 10, 100, 1000)) +
        labs(title = "Global Distribution of Billionaires",
            subtitle = "By country of citizenship",
            caption = "Data: Forbes Billionaires List"
        ) +
        theme_minimal() +
        theme(axis.text = element_blank(),
            panel.grid = element_blank(),
            legend.position = "bottom")



}