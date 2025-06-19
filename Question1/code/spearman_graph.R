spearman_graph <- function(df){


    ggplot(df, aes(x = base_year, y = spearman_rho, colour = Gender)) +
        geom_line(linewidth = 0.5) +
        geom_point(size = 1, alpha = 0.8) +
        theme_minimal(base_size = 12) +
        scale_colour_manual(values = c("Girls" = "deeppink", "Boys" = "steelblue")) +
        labs(
            title = "Naming Persistence for Girls and Boys",
            subtitle = "Spearman Correlation of Top 25 Names vs. Following 3 Years",
            x = "Base Year", y = "Spearman Rank Correlation (ρ)",
            colour = "Gender"
        ) +
        ylim(0, 1) +
        theme(
            plot.title = element_text(size = 14, face = "bold"),
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 10)
        )
}






#
#
#
#     # Set gender-specific plot colour
#     gender_label <- ifelse(gender_filter == "F", "Girls", "Boys")
#     line_colour <- ifelse(gender_filter == "F", "deeppink", "steelblue")
#
#     # create the ggplot
#     g <- ggplot(cor_results, aes(x = base_year, y = spearman_rho)) +
#         geom_line(colour = line_colour, linewidth = 1) +
#         geom_point(size = 2, alpha = 0.8) +
#         theme_minimal(base_size = 12) +
#         labs(
#             title = glue::glue("Naming Persistence for {gender_label}"),
#             subtitle = glue::glue("Spearman Correlation of Top {top_n} Names vs. Next {lag_n} Years"),
#             x = "Base Year", y = "Spearman Rank Correlation (ρ)"
#         ) +
#         ylim(0, 1) +
#         theme(
#             plot.title = element_text(size = 14, face = "bold"),
#             axis.title = element_text(size = 12),
#             axis.text = element_text(size = 10)
#         )
#
#     g
# }# create the ggplot
# g <- ggplot(cor_results, aes(x = base_year, y = spearman_rho)) +
#     geom_line(colour = line_colour, linewidth = 1) +
#     geom_point(size = 2, alpha = 0.8) +
#     theme_minimal(base_size = 12) +
#     labs(
#         title = glue::glue("Naming Persistence for {gender_label}"),
#         subtitle = glue::glue("Spearman Correlation of Top {top_n} Names vs. Next {lag_n} Years"),
#         x = "Base Year", y = "Spearman Rank Correlation (ρ)"
#     ) +
#     ylim(0, 1) +
#     theme(
#         plot.title = element_text(size = 14, face = "bold"),
#         axis.title = element_text(size = 12),
#         axis.text = element_text(size = 10)
#     )



