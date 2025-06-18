# plot the top movie producing countries

top10_plot <- function(df, col_name){

top_group <- df %>%
        filter(type == "MOVIE") %>%
        count(!!sym(col_name), name = "total_movies") %>%
        arrange(desc(total_movies)) %>%
        head(10)  # Select top 10

     # create the ggplot

     g <- ggplot(top_group,
            aes(x = reorder(!!sym(col_name), total_movies), #the !!sym() fixes the col reference issue!
                y = total_movies,
                fill = total_movies)) +  # Color by count
         geom_col() +
         geom_text(aes(label = total_movies), hjust = -0.1, size = 3) +  # Add counts
         coord_flip() +  # Horizontal bars
         scale_fill_gradient(low = "lightblue", high = "darkblue") +  # Color gradient
         labs(title = "Top 10 Countries by Movie Production (Movies only)",
              x = "",
              y = "Number of Movies") +
         theme_minimal() +
         theme(legend.position = "none",
               plot.title = element_text(face = "bold"))
g



}
