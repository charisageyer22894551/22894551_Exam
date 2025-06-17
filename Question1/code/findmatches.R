# # top 3 increased function
#
# findmatches <- function(df, gender_filter = "F", top_n = 25) {
#
# top_babynames_decade <- df %>%
#         filter(Gender == gender_filter) %>%
#         mutate(decade = floor(year / 10) * 10) %>%
#         group_by(decade, name) %>%
#         summarise(Total = sum(Count, na.rm = T), .groups = "drop") %>%
#         arrange(name, decade) %>%
#         group_by(name) %>%
#         mutate(perc_change = (Total - lag(Total)) / lag(Total) * 100) %>%
#         ungroup() %>%
#         group_by(decade) %>%
#         slice_max(order_by = perc_change, n = top_n, with_ties = FALSE) %>%
#         ungroup()
#
# ## find matches!
#
# # actor names
# HBO_actors <- HBO_MF %>%
#     mutate(first_name = str_to_lower(word(name, 1))) %>%
#     group_by(first_name) %>%
#     summarise(avg_score = mean(tmdb_score, na.rm = TRUE), appearances = n(), .groups = "drop") %>%
#     slice_max(avg_score, n = 50)  # Top N actors by TMDB score!
#
# # Character names
# HBO_characters <- HBO_characters_MF %>%
#     mutate(first_name = str_to_lower(word(name, 1))) %>%
#     group_by(first_name) %>%
#     summarise(avg_score = mean(tmdb_score, na.rm = TRUE), appearances = n(), .groups = "drop") %>%
#     slice_max(avg_score, n = 50) # again we only want to look at those that are popular!
#     # distinct(id, first_name)
#
# #get top 3 baby names and make it
# top_baby_names <- top_babynames_decade %>%
#     mutate(name = str_to_lower(name))
#
# # Match actors to Top 3 baby names
# actor_matches <- HBO_actors %>%
#     inner_join(top_baby_names, by = c("first_name" = "name"))
#
# # Match characters to Top 3 baby names
# character_matches <- HBO_characters %>%
#     inner_join(top_baby_names, by = c("first_name" = "name"))
#
# return(list(
#     actors = actor_matches,
#     characters = character_matches
# ))
# }

findmatches <- function(df, gender_filter = "F", top_n = 25) {

    # Step 1: Get top increasing baby names per decade
    top_babynames_decade <- df %>%
        filter(Gender == gender_filter) %>%
        mutate(decade = floor(year / 10) * 10) %>%
        group_by(decade, name) %>%
        summarise(Total = sum(Count, na.rm = TRUE), .groups = "drop") %>%
        arrange(name, decade) %>%
        group_by(name) %>%
        mutate(perc_change = (Total - lag(Total)) / lag(Total) * 100) %>%
        ungroup() %>%
        group_by(decade) %>%
        slice_max(order_by = perc_change, n = top_n, with_ties = FALSE) %>%
        ungroup() %>%
        mutate(name_clean = str_to_lower(name) %>% str_remove_all("[^a-z]"))

    # Step 2: Top actors
    HBO_actors <- HBO_MF %>%
        mutate(decade = floor(release_year / 10) * 10,
               first_name = str_to_lower(word(name, 1)) %>% str_remove_all("[^a-z]")) %>%
        group_by(decade, first_name) %>%
        summarise(avg_score = mean(tmdb_score, na.rm = TRUE), .groups = "drop") %>%
        slice_max(avg_score, n = 50)

    # Step 3: Top characters
    HBO_characters <- HBO_characters_MF %>%
        mutate(decade = floor(year / 10) * 10,
               first_name = str_to_lower(word(name, 1)) %>% str_remove_all("[^a-z]")) %>%
        group_by(decade, first_name) %>%
        summarise(avg_score = mean(tmdb_score, na.rm = TRUE), .groups = "drop") %>%
        slice_max(avg_score, n = 50)

    # Step 4: Join on decade and cleaned name
    actor_matches <- inner_join(HBO_actors, top_babynames_decade,
                                by = c("first_name" = "name_clean", "decade" = "decade"))

    character_matches <- inner_join(HBO_characters, top_babynames_decade,
                                    by = c("first_name" = "name_clean", "decade" = "decade"))

    return(list(
        actors = actor_matches,
        characters = character_matches
    ))
}