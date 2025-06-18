remove_special <- function(df){

    df <- df %>%
        mutate(across(where(is.character), ~ gsub("[^\x20-\x7E]", "", .)))
}