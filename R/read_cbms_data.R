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
read_cbms_data <- function(
  .path,
  .input_data = 'hp',
  .dictionary = NULL,
  .valueset = NULL,
  ...
) {

  if(.input_data == 'hp') {

    df <- readr::read_delim(
      .path,
      delim = "\t",
      quote = "",
      progress = F,
      trim_ws = T,
      show_col_types = F,
      ...
    )

  } else if (.input_data == 'bp') {

    df <- readr::read_csv(
      .path,
      progress = F,
      trim_ws = T,
      show_col_types = F,
      ...
    )

  } else {

    stop('Invalid input data.')

  }

  df <- df |>
    harmonize_variable(.dictionary) |>
    add_metadata(.dictionary, .valueset)

  class(df) <- c('rcbms_df', class(df))

  return(df)

}
