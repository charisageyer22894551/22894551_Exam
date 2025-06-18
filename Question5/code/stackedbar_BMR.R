    stackedbar_BMR <- function(df, category_col) {

        ggplot(df, aes(x = !!sym(category_col), fill = as.factor(healthy_bmr))) +
            geom_bar(position = "fill") +
            labs(
                title = glue::glue("Proportion of Healthy BMR by {category_col}"),
                x = rlang::as_label(category_col),
                y = "Proportion",
                fill = "Healthy BMR"
            ) +
            scale_y_continuous(labels = scales::percent_format()) +
            theme_minimal()
    }
