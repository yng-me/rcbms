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
    ) |> dplyr::select(-dplyr::starts_with('aux'))

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

  class(df) <- c('rcbms_df', class(df))

  return(df)

}
