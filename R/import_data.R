#' Rear CBMS Data
#'
#' @param .path
#' @param .input_data
#' @param .dictionary
#' @param ...
#'
#' @return
#' @export
#'
#' @examples

import_data <- function(.path, .input_data, .config, ...) {

  if(!(.input_data %in% c("hp", "bp", "ilq", "cph", "bs"))) {
    stop('Invalid input data.')
  }

  file_format <- .config$project[[.input_data]]$file_format
  import_func <- eval(as.name(paste0("import_", file_format)))

  df <- import_func(.path, ...)

  set_class(df, "rcbms_df")
}

import_txt <- function(.path, ...) {
  readr::read_delim(
    .path,
    delim = "\t",
    quote = "",
    progress = FALSE,
    trim_ws = TRUE,
    show_col_types = FALSE,
    ...
  ) |>
    dplyr::select(-dplyr::starts_with("aux")) |>
    convert_to_na() |>
    convert_to_na(.pattern = "^DEFAULT$")
}


import_csv <- function(.path, ...) {
  suppressMessages(
    readr::read_csv(
      .path,
      progress = FALSE,
      trim_ws = TRUE,
      show_col_types = FALSE,
      skip_empty_rows = TRUE,
      ...
    ) |>
      dplyr::select(-dplyr::any_of("...1")) |>
      convert_to_na()
  )
}

import_xlsx <- function(.path, ...) {
  openxlsx::read.xlsx(
    .path,
    skipEmptyRows = TRUE,
    skipEmptyCols = TRUE,
    ...
  ) |> convert_to_na()
}
