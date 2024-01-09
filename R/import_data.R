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
#'
import_data <- function(
  .path,
  .input_data = 'hp',
  ...
) {

  if(.input_data == 'hp') {

    df <- readr::read_delim(
        .path,
        delim = "\t",
        quote = "",
        progress = FALSE,
        trim_ws = TRUE,
        show_col_types = FALSE,
        ...
      ) |>
      dplyr::select(-dplyr::starts_with('aux'))

  } else if (.input_data == 'bp') {

    df <- readr::read_csv(
        .path,
        progress = FALSE,
        trim_ws = TRUE,
        show_col_types = FALSE,
        skip_empty_rows = TRUE,
        ...
      ) |>
      dplyr::select(-dplyr::any_of("...1")) |>
      convert_to_na()

  } else {

    stop('Invalid input data.')

  }

  set_class(df, "rcbms_df")

  return(df)

}

