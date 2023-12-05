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

  if(!is.null(.dictionary)) {

    required_cols <- c('variable_name_new', 'type', 'length')
    valid_cols <- which(required_cols %in% names(.dictionary))

    if(length(valid_cols) < length(required_cols)) {
      stop('Invalid data dictionary.')
    }

    df <- df |> harmonize_variable(.dictionary)
  }

  return(df)
}












