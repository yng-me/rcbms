add_metadata <- function(.data, selected_input) {

  df_name <- names(.data)

  if(!exists('mode_type')) {
    mode_type <- 'validation'
  }

  dcf <- refs[[selected_input]][['data_dictionary']] %>%
    convert_to_na() %>%
    distinct(value, .keep_all = T) %>%
    mutate(mode = mode_type) %>%
    mutate(
      variable = if_else(
        is.na(new_variable_name),
        str_trim(value),
        str_trim(new_variable_name)
      ),
      variable = if_else(
        mode == 'validation',
        value,
        variable
      )
    ) %>%
    mutate(label = if_else(is.na(item), label, paste0(item, ' - ', label))) %>%
    select(variable, label, any_of('valueset')) %>%
    distinct(.keep_all = T)  %>%
    filter(variable %in% df_name, !is.na(label))

  for(i in seq_along(dcf$variable)) {

    attr(.data[[dcf$variable[i]]], 'label') <- dcf$label[i]
  }

  if('barangay_geo' %in% df_name) {
    attr(.data$barangay_geo, 'label') <- 'Barangay Geo ID'
  }

  if('valueset' %in% names(dcf)) {

    for(j in seq_along(dcf$variable)) {

      vs <- refs[[selected_input]][['valueset']] %>%
        filter(list_name == dcf$valueset[j]) %>%
        select(-list_name)

      if(nrow(vs) > 0) {

        if(grepl('\\d+', vs$value[1])) {
          vs <- vs %>%
            mutate(value = as.integer(value))
        }

        attr(.data[[dcf$variable[j]]], 'valueset') <- vs %>%
          mutate(label = paste0(value, ' - ', label))

      }
    }
  }

  return(.data %>% select(-contains(c('aux', 'lnoctr'))))

}
