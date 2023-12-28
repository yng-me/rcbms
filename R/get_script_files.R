#' Get script files
#'
#' @param .input_data
#' @param .config
#'
#' @return
#' @export
#'
#' @examples
#'
get_script_files <- function(.input_data, .config = getOption('rcbms_config')) {

  script_files <- list.files(
    join_path(.config$base, 'scripts', .config$mode$type, .input_data),
    pattern = '\\.(r|R)$',
    full.names = T
  )

  if(length(script_files) == 0) return(NULL)

  script_files <- script_files |>
    dplyr::as_tibble() |>
    dplyr::mutate(s = seq(1:dplyr::n())) |>
    dplyr::mutate(s = dplyr::if_else(grepl('__', value), 0L, s)) |>
    dplyr::arrange(s, value) |>
    dplyr::select(value, order = s) |>
    dplyr::mutate(title = stringr::str_remove(basename(value), '\\.(r|R)$')) |>
    dplyr::mutate(
      title = paste0(
        toupper(stringr::str_sub(title, 1, 1)),
        stringr::str_remove(stringr::str_sub(title, 2, -1), '-[a-z0-1]$'), ' ',
        toupper(stringr::str_sub(title, -1))
      ),
      input_data = .input_data
    ) |>
    dplyr::rename(file = value) |>
    dplyr::select(input_data, file, order)

  return(script_files)

}
