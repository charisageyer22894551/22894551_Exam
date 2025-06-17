# extract first name function and add the gender


add_gender <- function(df) {

    # prepare the lookup file
    Name_Gender_Lookup <- Baby_Names %>%
    mutate(name = str_to_lower(name)) %>%
    group_by(name, Gender) %>%
    summarise(Total = sum(Count), .groups = "drop") %>%
    group_by(name) %>%
    slice_max(order_by = Total, n = 1) %>%
    ungroup()


# let's get only the first names
df %>%
    mutate(First_Name = (word(name, 1))) %>%
    left_join(Name_Gender_Lookup, by = c("First_Name" = "name"))
}

# df_gendered <- df %>%
#     left_join(Name_Gender_Lookup, by = c("First_Name" = "Name"))
# }

