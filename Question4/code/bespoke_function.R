# bespoke funciton based on info in excel file

# treat each column as the type it is

bespoke_function <- function(file_path) {

df <- read_csv(
    file_path,
    col_types = cols(
        .default = col_character(),
        rank = col_integer(),
        year = col_integer(),
        `company.founded` = col_integer(),
        `demographics.age` = col_integer(),
        `location.gdp` = col_number(),
        `wealth.worth in billions` = col_number()
    ),
    locale = locale(decimal_mark = ",", grouping_mark = ".")
) # Change the , in the decimals to .

df %>% mutate(decade = floor(year / 10) * 10,
              self_made = grepl("not inherited", wealth.how.inherited, ignore.case = T),
              demographics.age = ifelse(`demographics.age` < 0, NA, `demographics.age`)) %>%
    # just clean out the <0 value
    rename(gdp_country = `location.gdp`,
           net_worth = `wealth.worth in billions`,
           industry = `wealth.how.industry`) %>%

    janitor::clean_names() %>%  # make everything_a_nice_format - don't have to rename all
    #ai but it changes the format again - let's change abck to type bc i want to keep the _names_like_this
    mutate(across(c(rank, year, company_founded, demographics_age), as.integer),
           across(c(gdp_country, net_worth), as.numeric))
}
