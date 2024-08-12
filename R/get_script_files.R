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

get_script_files <- function(.input_data, .section = NULL, .config = getOption("rcbms.config")) {

  script_files <- list.files(
    join_path(.config$base, "scripts", .config$mode$type, .input_data),
    pattern = "\\.(r|R)$",
    full.names = T
  )

  if (length(script_files) == 0) {
    return(NULL)
  }

  if(.config$mode$edit == 0 | .config$mode$edit == 4) {

    if(.config$mode$edit == 4) {
      grepl_file <- "^__[initial|signature]"
    } else if (.config$mode$edit == 0) {
      grepl_file <- "^__[initial|preliminary]"
    }

    script_files <- script_files |>
      dplyr::as_tibble() |>
      dplyr::mutate(title = stringr::str_remove(basename(tolower(value)), '\\.(r|R)$')) |>
      dplyr::filter(grepl(grepl_file, title)) |>
      dplyr::mutate(order = seq(1:dplyr::n())) |>
      dplyr::mutate(order = order + 1) |>
      dplyr::mutate(order = dplyr::if_else(grepl('^__*', value), 0L, order)) |>
      dplyr::mutate(order = dplyr::if_else(grepl('^_.*', value), 1L, order)) |>
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

    section_ref <- .section[[.config$survey_round]][[.input_data]]
    selected_scripts <- NULL
    selected_section_ext <- NULL
    script_files_final <- NULL

    if(!is.null(section_ref)) {

      selected_scripts <- section_ref |>
        dplyr::filter(included, builtin_included) |>
        dplyr::pull(validation.script_file) |>
        unlist() |>
        unique()

      selected_section_ext <- section_ref |>
        dplyr::filter(included, extension_included) |>
        dplyr::pull(value)

      if(length(selected_section_ext) > 0) {

        selected_ext <- file.path(
          .config$base,
          'extensions',
          .config$mode$type,
          .input_data,
          selected_section_ext
        )

        ext_scripts <- list.files(
          selected_ext,
          pattern = "\\.(r|R)$",
          full.names = T
        )
      }
    }

    if(!is.null(.section)) {

      start_files_grep <- 'initial'
      if(.config$mode$stage == 1 & .config$mode$edit == 0) {
        start_files_grep <- 'initial|preliminary'
      } else {
        start_files_grep <- 'initial'
      }

      script_files_final <- script_files |>
        dplyr::mutate(name = stringr::str_remove_all(basename(file), '\\.R$')) |>
        dplyr::filter(name %in% selected_scripts | grepl(start_files_grep, name)) |>
        dplyr::select(-name)
    }

    if(length(selected_section_ext) > 0) {

      ext_scripts <- ext_scripts |>
        dplyr::as_tibble() |>
        dplyr::rename(file = value) |>
        dplyr::mutate(
          input_data = .input_data,
          order = nrow(script_files) + 10
        )

      if(!is.null(script_files_final)) {

        script_files_final <- script_files_final |>
          dplyr::bind_rows(ext_scripts)

      }
    }

    if(.config$validation$include_prelim) {
      script_files_final <- dplyr::tibble(

        ) |>
        dplyr::bind_rows(script_files_final)

    }

    if(.config$validation$include_signature) {


    }

  }


  return(script_files_final)
}
