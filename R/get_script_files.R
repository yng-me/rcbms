#' Get script files
#'
#' @param .config
#' @param .input_data
#'
#' @return
#' @export
#'
#' @examples
#'
get_script_files <- function(.config = getOption('rcbms_config'), .input_data = 'hp') {

  script_files <- list.files(
    join_path(.config$base, 'scripts', .config$mode$type, .input_data),
    pattern = '\\.(r|R)$',
    full.names = T
  ) |>
  dplyr::as_tibble() |>
  dplyr::mutate(s = seq(1:dplyr::n())) |>
  dplyr::mutate(s = dplyr::if_else(grepl('__', value), 0L, s)) |>
  dplyr::arrange(s, value) |>
  dplyr::select(value) |>
  dplyr::mutate(title = stringr::str_remove(basename(value), '\\.(r|R)$')) |>
  dplyr::mutate(
    title = paste0(
      toupper(stringr::str_sub(title, 1, 1)),
      stringr::str_remove(stringr::str_sub(title, 2, -1), '-[a-z0-1]$'), ' ',
      toupper(stringr::str_sub(title, -1))
    )
  ) |>
  dplyr::rename(file = value)

  return(script_files$file)

}
