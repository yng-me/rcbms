#' Harmonize variable
#'
#' @param .data
#' @param .dictionary
#' @param .survey_round
#' @param .input_data
#' @param .config
#'
#' @return
#' @export
#'
#' @examples
#'
harmonize_variable <- function(.data, .input_data, .survey_round, .dictionary, .config) {

  if(is.null(.dictionary)) return(.data)

  validate_required_cols(
    .dictionary,
    c('variable_name', 'variable_name_new', 'valueset', 'label')
  )

  .dictionary <- .dictionary |>
    dplyr::mutate(
      variable_name = tolower(stringr::str_trim(variable_name)),
      type = stringr::str_trim(type),
      length = as.integer(length)
    )

  excluded <- .dictionary |>
    dplyr::filter(is_included != 1) |>
    dplyr::pull(variable_name)

  .data |>
    dplyr::select(-dplyr::any_of(excluded)) |>
    convert_cols_from_dictionary(.dictionary) |>
    convert_col_names(.dictionary)
}


join_data_with_dictionary <- function(.data, .dictionary) {
  dplyr::as_tibble(names(.data)) |>
    dplyr::rename(variable_name = value) |>
    dplyr::left_join(
      dplyr::distinct(.dictionary, variable_name, .keep_all = T),
      by = "variable_name",
      multiple = "first"
    )
}

convert_cols_from_dictionary <- function(.data, .dictionary) {
  get_col_type <- function(.type) {
    as_type <- .dictionary$variable_name[.dictionary$type == .type]
    nc_names <- as_type[as_type %in% names(.data)]
    return(nc_names)
  }

  # convert to character
  as_char <- get_col_type("c")
  if (length(as_char) > 0) {
    .data <- .data |>
      dplyr::mutate_at(
        dplyr::vars(dplyr::any_of(as_char)),
        as.character
      )
  }

  # convert to double
  as_dbl <- get_col_type("d")
  if (length(as_dbl) > 0) {
    .data <- .data |>
      dplyr::mutate_at(
        dplyr::vars(dplyr::any_of(as_dbl)),
        as.double
      )
  }

  # convert to integer
  as_int <- get_col_type("i")
  if (length(as_int) > 0) {
    .data <- .data |>
      dplyr::mutate_at(
        dplyr::vars(dplyr::any_of(as_int)),
        as.integer
      )
  }

  # convert to numeric character
  as_nc <- get_col_type("nc")
  for (i in seq_along(as_nc)) {
    nc <- as_nc[i]
    .data <- .data |>
      dplyr::mutate(
        !!as.name(nc) := stringr::str_pad(
          as.integer(!!as.name(nc)),
          pad = "0",
          width = .dictionary$length[.dictionary$variable_name == nc][1]
        )
      )
  }

  # convert to ISO date
  as_date_col <- get_col_type("D")
  if (length(as_date_col) > 0) {
    .data <- .data |>
      dplyr::mutate_at(
        dplyr::vars(dplyr::any_of(as_date_col)),
        lubridate::ymd
      )
  }

  # convert to year
  as_year <- get_col_type("y")
  if (length(as_year) > 0) {
    .data <- .data |>
      dplyr::mutate_at(
        dplyr::vars(dplyr::any_of(as_year)),
        ~ lubridate::year(as.Date(as.character(.), format = "%Y"))
      )
  }

  # convert to month
  as_month <- get_col_type("m")
  if (length(as_month) > 0) {
    .data <- .data |>
      dplyr::mutate_at(
        dplyr::vars(dplyr::any_of(as_month)),
        ~ lubridate::month(as.Date(as.character(.), format = "%m"))
      )
  }

  # convert to month
  as_time <- get_col_type("t")

  if (length(as_time) > 0) {
    .data <- .data |>
      dplyr::mutate_at(
        dplyr::vars(dplyr::any_of(as_time)),
        as.character
      )
  }

  from_dcf <- .dictionary$variable_name
  from_data_without_dcf <- setdiff(names(.data), from_dcf)

  if (length(from_data_without_dcf) > 0) {
    if (getOption("rcbms.config")$verbose) {
      cli::cli_alert(
        cli::col_br_red("No matching data dictionary entries and will be coerced as character type:")
      )
      cli::cli_ul(from_data_without_dcf)
    }

    .data <- .data |>
      dplyr::mutate_at(dplyr::vars(dplyr::any_of(from_data_without_dcf)), as.character)
  }

  return(.data)
}

convert_col_names <- function(.data, .dictionary) {
  df_names <- .data |>
    join_data_with_dictionary(.dictionary) |>
    dplyr::mutate(
      variable_name = dplyr::if_else(
        is.na(variable_name_new),
        variable_name,
        stringr::str_trim(variable_name_new)
      )
    ) |>
    dplyr::group_by(variable_name) |>
    dplyr::mutate(n = 1:dplyr::n()) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      variable_name = dplyr::if_else(
        n == 1,
        variable_name,
        paste0(variable_name, "_", n)
      )
    )

  colnames(.data) <- df_names$variable_name

  return(.data)
}


#' Title
#'
#' @param .data
#'
#' @return
#' @export
#'
#' @examples
normalize_variable <- function(.data) {
  .data |>
    dplyr::rename_with(~ stringr::str_remove(., "^[a-e]\\d{2}_([a-z]_)?"))
}


#' Title
#'
#' @param .data
#'
#' @return
#' @export
#'
#' @examples
normalise_variable <- function(.data) {
  .data |>
    dplyr::rename_with(~ stringr::str_remove(., "^[a-e]\\d{2}_([a-z]_)?"))
}
