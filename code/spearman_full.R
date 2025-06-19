spearman_full <- function(df){

        spearman_F <- df %>% filter(Gender == "F") %>% spearman_analysis() %>%  mutate(Gender = "Girls")
    spearman_M <- df %>% filter(Gender == "M") %>% spearman_analysis() %>%   mutate(Gender = "Boys")

    spearman_all <- bind_rows(spearman_M, spearman_F)

    spearman_graph(spearman_all)

}