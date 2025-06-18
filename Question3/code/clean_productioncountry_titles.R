clean_production_countries <- function(df) {
    df %>%
       filter(!production_countries %in% c("", "[]", "NA")) %>%
        mutate(production_countries = production_countries %>%
                gsub("\\[|\\]|'|\"", "", .) %>%
                gsub("\\s*,\\s*", ", ", .) ) %>%

        separate_rows(production_countries, sep = ",\\s*") %>%  # Split into one row per country

        mutate(production_countries = trimws(production_countries))   # Trim whitespace thats bugging
}

#
#     gsub("\\bUS\\b", "United States", .) %>%
#     gsub("\\bGB\\b", "United Kingdom", .) %>%
#     gsub("\\bIN\\b", "India", .) deal with this later!