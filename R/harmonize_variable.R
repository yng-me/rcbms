#' Harmonize variable
#'
#' @param .data
#' @param .dictionary
#'
#' @return
#' @export
#'
#' @examples
#'
harmonize_variable <- function(.data, .dictionary) {

  excluded <- .dictionary |>
    dplyr::filter(is_included != 1) |>
    dplyr::pull(variable_name)

  .data <- .data |>
    dplyr::select(-dplyr::any_of(tolower(excluded))) |>
    clean_colnames() |>
    convert_col_types(.dictionary) |>
    convert_col_names(.dictionary)

}


convert_col_types <- function(.data, .dictionary) {

  cols_with_type <- .data |>
    join_data_with_dictionary(.dictionary) |>
    dplyr::transmute(
      name = stringr::str_trim(variable_name),
      type = stringr::str_trim(type),
      length = as.integer(length)
    ) |>
    convert_to_na() |>
    dplyr::mutate(type = dplyr::if_else(type == 'nc', 'c', type)) |>
    dplyr::mutate(type = dplyr::if_else(is.na(type), '?', type))

  .data |>
    # readr::type_convert(
    #   col_types = paste0(cols_with_type$type, collapse = '')
    # ) |>
    convert_col_numeric_character(.dictionary)

}

join_data_with_dictionary <- function(.data, .dictionary) {

  dplyr::as_tibble(names(.data)) |>
    dplyr::left_join(
      dplyr::distinct(.dictionary, variable_name, .keep_all = T),
      by = dplyr::join_by(value == variable_name)
    ) |>
    dplyr::rename(variable_name = value) |>
    convert_to_na()

}

convert_col_names <- function(.data, .dictionary) {

  df_names <- .data |>
    join_data_with_dictionary(.dictionary) |>
    dplyr::mutate(
      variable = dplyr::if_else(
        is.na(variable_name_new),
        variable_name,
        stringr::str_trim(variable_name_new)
      )
    ) |>
    dplyr::group_by(variable) |>
    dplyr::mutate(n = 1:dplyr::n()) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      variable = dplyr::if_else(
        n == 1,
        variable,
        paste0(variable, '_', n)
      )
    )

  colnames(.data) <- df_names$variable

  return(.data)

}

convert_col_numeric_character <- function(.data, .dictionary) {

  as_nc <- .dictionary |>
    dplyr::filter(
      stringr::str_trim(type) == 'nc',
      variable_name_new %in% names(.data)
    ) |>
    dplyr::mutate(length = as.integer(length)) |>
    dplyr::select(name = variable_name_new, type, length)

  as_int <- .dictionary |>
    dplyr::filter(stringr::str_trim(type) == 'i') |>
    dplyr::select(name = variable_name_new)

  .data <- .data |>
    dplyr::mutate_at(
      dplyr::vars(dplyr::any_of(as_int$name)),
      as.integer
    )

  print(as_nc$name)

  for(i in seq_along(as_nc$name)) {
    nc <- as_nc$name[i]
    print(nc)
    .data <- .data |>
      dplyr::mutate(
        !!as.name(nc) := stringr::str_pad(
          as.integer(!!as.name(nc)),
          pad = '0',
          width = as_nc$length[as_nc$name == nc][1]
        )
      )
  }
  return(.data)
}
