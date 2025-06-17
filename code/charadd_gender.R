charadd_gender <- function(df) {
    Name_Gender_Lookup <- Baby_Names %>%
        mutate(name = str_to_lower(iconv(name, "UTF-8", "UTF-8", sub = ""))) %>%  # Clean encoding
        count(name, Gender, wt = Count, name = "Total") %>%
        slice_max(Total, n = 1, by = name, with_ties = FALSE)

    df %>%
        mutate(
            name_clean = str_to_lower(iconv(name, "UTF-8", "UTF-8", sub = "")),  # Clean encoding
            Gender = map_chr(name_clean, ~ {
                matches <- Name_Gender_Lookup %>%
                    filter(str_detect(.x, fixed(name)))
                if (nrow(matches) > 0) matches$Gender[1] else NA_character_
            })
        ) %>%
        select(-name_clean)
}