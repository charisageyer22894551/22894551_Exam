LPM <- function(df) {
    library(ggthemes)
    library(tidyverse)
    library(ggthemes)
    library(broom)

   df <- df %>%
        mutate(across(c(`Physical Activity Level`, `Sleep Quality`, `Stress Level`), as.factor))

    models <- df %>%
        group_by(`Gender`) %>%   # So we estimate a model for gender
        regress(healthy_bmr ~ ., m("lm"))
    models

    factor_loadings <- coef(models)

    plot_data <- factor_loadings %>%
        mutate(term = str_remove(term, "Sleep Quality|Physical Activity Level"), term = gsub("Current Weight (Ibs)", "Weight (lbs)", term), Gender = factor(Gender, levels = c("F", "M"), labels = c("Female", "Male"))) %>%
        filter(term != "(Intercept)")

    # Create the plot
    g <- ggplot(plot_data, aes(x = estimate, y = reorder(term, estimate), color = Gender)) +
        geom_point(size = 3) +
        geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
        facet_wrap(~Gender, scales = "free_y") +
        # scale_color_economist() +
        labs(
            x = "Regression Coefficient",
            y = "Predictor",
            title = "Gender-Specific Effects on Healthy BMR"
        ) +
        # theme_economist() +
        theme(legend.position = "none")
    g
}