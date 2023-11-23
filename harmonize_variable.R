harmonize_variable <- function(.data, dictionary) {

  dcf <- dictionary %>%
    distinct(value, .keep_all = T)

  excluded <- dcf %>%
    filter(is_included != 1) %>%
    pull(value)

  .data <- .data %>%
    select(-matches(c(paste0('^', tolower(excluded), '$'))))

  df_name <- as_tibble(names(.data)) %>%
    left_join(dcf, by = 'value') %>%
    convert_to_na() %>%
    mutate(
      variable = if_else(
        is.na(new_variable_name),
        value,
        str_trim(new_variable_name)
      )
    ) %>%
    group_by(variable) %>%
    mutate(n = 1:n()) %>%
    ungroup() %>%
    mutate(
      variable = if_else(
        n == 1,
        variable,
        paste0(variable, '_', n)
      )
    )

  colnames(.data) <- df_name$variable

  return(.data)

}
