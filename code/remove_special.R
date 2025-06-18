
# remove_special <- function(df, col_name) {
#     df %>%
#         mutate(across({{col_name}}, iconv, from = "UTF-8", to = "ASCII", sub = ""))
# }

remove_special <- function(df, col_name) {
    df %>%
        mutate(col_name = iconv(col_name, from = "UTF-8", to = "ASCII", sub = ""))
 }
# doesnt pick up col_name! need to specify with {{}}. Easeir to just use across

#
# remove_special <- function(df, ...) {
#     df %>%
#         mutate(across(c(...), ~iconv(., from = "UTF-8", to = "ASCII", sub = "")))
# } #that gives me error now :( )

# even nicer with purr :) now I can use it for many cols at a time