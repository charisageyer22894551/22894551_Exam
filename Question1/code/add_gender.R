# extract first name function and add the gender


add_gender <- function(df, name_col) {

    # prepare the lookup file
    Name_Gender_Lookup <- Baby_Names %>%
    mutate(Name = str_to_lower(Name)) %>%
    group_by(Name, Gender) %>%
    summarise(Total = sum(Count), .groups = "drop") %>%
    group_by(Name) %>%
    slice_max(order_by = Total, n = 1) %>%
    ungroup()


# let's get only the first names
df <- df %>%
    mutate(First_Name = str_to_lower(word({{ name_col }}, 1))) %>%
    left_join(Name_Gender_Lookup, by = c("First_Name" = "Name"))
}

# df_gendered <- df %>%
#     left_join(Name_Gender_Lookup, by = c("First_Name" = "Name"))
# }
