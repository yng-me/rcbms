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

  script_files_all <- list.files(
      join_path(.config$base, "scripts", .config$mode$type, .input_data),
      pattern = "\\.(r|R)$",
      full.names = T
    ) |>
    dplyr::as_tibble() |>
    dplyr::mutate(title = stringr::str_remove(basename(tolower(value)), '\\.(r|R)$')) |>
    dplyr::mutate(order = seq(1:dplyr::n())) |>
    dplyr::mutate(order = order + 2) |>
    dplyr::arrange(order, value) |>
    dplyr::mutate(order = dplyr::if_else(grepl('^__initial', value), 0L, order)) |>
    dplyr::mutate(order = dplyr::if_else(grepl('^__*', value), 1L, order)) |>
    dplyr::mutate(order = dplyr::if_else(grepl('^_.*', value), 2L, order)) |>
    dplyr::transmute(
      input_data = .input_data,
      file = value,
      order
    )

  if (length(script_files_all) == 0) {
    return(NULL)
  }


  # Prelim
  if(.config$mode$edit == 0) {

    script_files_final <- script_files_all |>
      dplyr::filter(
        grepl(
          "^(__initial|_preliminary|_duplicate)",
          stringr::str_remove(basename(tolower(file)), '\\.(r|R)$')
        )
      )

  # Signature
  } else if (.config$mode$edit == 3) {

    script_files_final <- script_files_all |>
      dplyr::filter(
        grepl(
          "^(__initial|_signature)",
          stringr::str_remove(basename(tolower(file)), '\\.(r|R)$')
        )
      )

  # Others
  }  else {

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

      script_files_final <- script_files_all |>
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
          order = nrow(script_files_all) + 10
        ) |>
        dplyr::bind_rows(
          script_files_all |>
            dplyr::filter(
              grepl(
                "^__initial",
                stringr::str_remove(basename(tolower(file)), '\\.(r|R)$')
              )
            )
        )

      if(!is.null(script_files_final)) {

        script_files_final <- script_files_final |>
          dplyr::bind_rows(ext_scripts)

      }
    }

    if(.config$validation$include_prelim) {
      script_files_final <- script_files_all |>
        dplyr::filter(
          grepl(
            "^(__initial|_preliminary|_duplicate)",
            stringr::str_remove(basename(tolower(file)), '\\.(r|R)$')
          )
        ) |>
        dplyr::bind_rows(script_files_final)

    }

    if(.config$validation$include_signature) {
      script_files_final <- script_files_all |>
        dplyr::filter(
          grepl(
            "^(__initial|_signature)",
            stringr::str_remove(basename(tolower(file)), '\\.(r|R)$')
          )
        ) |>
        dplyr::bind_rows(script_files_final)

    }
  }

  script_files_final |>
    dplyr::distinct(.keep_all = T) |>
    dplyr::arrange(order, file)

}
