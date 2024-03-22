#' Get script files
#'
#' @param .input_data
#' @param .config
#' @param .section
#'
#' @return
#' @export
#'
#' @examples
#'
get_script_files <- function(.input_data, .section = NULL, .config = getOption('rcbms.config')) {

  script_files <- list.files(
    join_path(.config$base, 'scripts', .config$mode$type, .input_data),
    pattern = '\\.(r|R)$',
    full.names = T
  )

  if(length(script_files) == 0) return(NULL)

  if(.config$mode$edit == 0) {

    script_files <- script_files |>
      dplyr::as_tibble() |>
      dplyr::mutate(title = stringr::str_remove(basename(tolower(value)), '\\.(r|R)$')) |>
      dplyr::filter(grepl('^__[initial|preliminary]', title)) |>
      dplyr::mutate(order = seq(1:dplyr::n())) |>
      dplyr::mutate(order = dplyr::if_else(grepl('__', value), 0L, order)) |>
      dplyr::transmute(
        input_data = .input_data,
        file = value,
        order
      )

  } else {

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

    selected_scripts <- .section[[.config$survey_round]][[.input_data]] |>
      dplyr::filter(included) |>
      dplyr::pull(validation.script_file) |>
      unlist() |>
      unique()

    if(!is.null(.section) & length(selected_scripts) > 0) {
      script_files <- script_files |>
        dplyr::mutate(name = stringr::str_remove_all(basename(file), '\\.R$')) |>
        dplyr::filter(name %in% selected_scripts | grepl('initial', name)) |>
        dplyr::select(-name)
    }

  }

  return(script_files)

}
