thresholdBMR <- function(df){

        df_lpm <- df %>%
        mutate(healthy_bmr = ifelse(
            `BMR (Calories)` / `Current Weight (lbs)`*2.2 > 25 &
                `BMR (Calories)` / `Current Weight (lbs)`*2.2 < 33, 1, 0)) %>%
        select(`Sleep Quality`, `Physical Activity Level`, `Stress Level`, `Current Weight (lbs)`, `Age`, `Gender`, `healthy_bmr`)

}