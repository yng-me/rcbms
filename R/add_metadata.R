#' Add metadata
#'
#' @param .data
#' @param .dictionary
#' @param .valueset
#'
#' @return
#' @export
#'
#' @examples
#'
add_metadata <- function(
  .data,
  .dictionary,
  .valueset,
  .survey_round,
  .input_data
) {

  if(is.null(.dictionary) & is.null(.valueset)) return(.data)
  validate_required_cols(
    .dictionary,
    c('variable_name', 'variable_name_new', 'valueset', 'label')
  )
  validate_required_cols(.valueset, c('name', 'value', 'label'))

  df_name <- names(.data)

  .dictionary <- .dictionary |>
    dplyr::filter(
      survey_round == as.integer(.survey_round),
      input_data == .input_data
    ) |>
    dplyr::collect() |>
    convert_to_na() |>
    dplyr::distinct(variable_name, .keep_all = T) |>
    dplyr::mutate(
      variable = dplyr::if_else(
        is.na(variable_name_new),
        stringr::str_trim(variable_name),
        stringr::str_trim(variable_name_new)
      )
    ) |>
    dplyr::select(variable, label, valueset) |>
    dplyr::distinct(.keep_all = T)  |>
    dplyr::filter(variable %in% df_name, !is.na(label))

  for(i in seq_along(.dictionary$variable)) {
    attr(.data[[.dictionary$variable[i]]], 'label') <- as.character(.dictionary$label[i])
  }

  if('barangay_geo' %in% df_name) {
    attr(.data$barangay_geo, 'label') <- 'Barangay Geo ID'
  }

  if('valueset' %in% names(.dictionary)) {

    for(j in seq_along(.dictionary$variable)) {

      vs <- .valueset |>
        dplyr::collect() |>
        dplyr::filter(name == .dictionary$valueset[j]) |>
        dplyr::select(-name)

      if(nrow(vs) > 0) {

        if(grepl('\\d+', vs$value[1])) {
          vs <- vs |>
            dplyr::mutate(value = as.integer(value))
        }

        attr(.data[[.dictionary$variable[j]]], 'valueset') <- vs

      }
    }
  }

  return(.data)

}
