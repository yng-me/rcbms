#' Generate validation output
#'
#' @param .cv
#' @param .cv_ref
#' @param .config
#' @param .section_ref
#'
#' @return
#' @export
#'
#' @examples
generate_validation <- function(.cv, .cv_ref, .config, .section_ref = NULL) {

  if(exists("CURRENT_INPUT_DATA")) {
    input_data <- CURRENT_INPUT_DATA
  } else {
    input_data <- .config$input_data[1]
  }

  pl <- tolower(stringr::str_trim((.config$validation$priority_level)))

  .cv_ref <- .cv_ref |>
    dplyr::filter(tolower(priority_level) %in% pl)

  result_names <- names(.cv)
  result_names <- result_names[result_names %in% .cv_ref$validation_id]

  with_incon <- NULL
  is_not_rcbms_cv_tbl <- NULL

  for (i in seq_along(result_names)) {
    result_name <- result_names[i]
    is_rcbms_cv_tbl <- inherits(.cv[[result_name]], "rcbms_cv_tbl")

    if (!is_rcbms_cv_tbl) {
      is_not_rcbms_cv_tbl <- c(is_not_rcbms_cv_tbl, result_name)
    }

    if (nrow(.cv[[result_name]]) > 0 && is_rcbms_cv_tbl) {
      with_incon <- c(with_incon, result_name)
    }
  }

  if (!is.null(is_not_rcbms_cv_tbl) & .config$verbose) {
    cli::cli_rule()
    cli::cli_text(
      cli::col_br_red(
        paste0(
          "The following validation results are not of class `rcbms_cv_tbl` object.\nPlease use ",
          cli::style_bold("select_cv()"), " for each validation check."
        )
      )
    )
    cli::cli_ul(
      paste0(
        cli::style_bold(is_not_rcbms_cv_tbl), " ",
        cli::col_br_red("✗")
      )
    )
    cli::cli_rule()
  }

  result_names <- result_names[result_names %in% with_incon]

  output <- NULL

  add_info <- .config$validation$include_additional_info
  include_contact_info <- .config$validation$include_contact_info

  if(is.null(include_contact_info)) {
    include_contact_info <- TRUE
  }

  uid <- .config$project[[input_data]]$id
  if (is.null(uid)) uid <- "case_id"

  output_list <- list()

  for (i in seq_along(result_names)) {

    result_name <- result_names[i]

    if(.config$verbose) {
      cat(paste0(str_pad(formatC(nrow(.cv[[result_name]]), big.mark = ','), width = 7), ': ', result_name, '\n'))
    }

    # if(.config$progress) {
      cli::cli_text(
        paste0(
        i, ' of ', length(result_names),
        ': ', result_name,
        ' (', formatC(nrow(.cv[[result_name]]), big.mark = ','), ')\n')
      )
    # }

    output_temp <- .cv[[result_name]] |>
      dplyr::mutate(validation_id = result_name)

    if (!("line_number" %in% names(output_temp)) & input_data %in% c("hp", "ilq")) {
      output_temp <- output_temp |>
        dplyr::mutate(line_number = NA_character_)
    }

    group_cols <- c('validation_id', uid)

    if("line_number" %in% names(output_temp))  {
      group_cols <- c(group_cols, 'line_number')
    }

    if(add_info) {
      output_list[[result_name]] <- output_temp |>
        dplyr::select(-dplyr::any_of(c("region", "province", "city_mun", "barangay", "ean"))) |>
        dplyr::group_by(dplyr::pick(dplyr::any_of(group_cols))) |>
        tidyr::nest(.key = "info") |>
        dplyr::ungroup() |>
        dplyr::tibble()

    } else {
      output_list[[result_name]] <- output_temp |>
        dplyr::select(dplyr::any_of(group_cols)) |>
        dplyr::tibble()
    }
  }

  if(length(output_list)) {

    if(add_info) {

      output <- output_list |>
        dplyr::bind_rows() |>
        dplyr::mutate(info = purrr::map_chr(info, \(x) {
          x |>
            dplyr::mutate_all(as.character) |>
            dplyr::mutate_all(~ dplyr::if_else(is.na(.), "Missing/NA", .)) |>
            jsonlite::toJSON() |>
            as.character() |>
            encrypt_info(.config)
        }))

    } else {
      output <- output_list |>
        dplyr::bind_rows()

    }

    if(include_contact_info) {
      output <- output |>
        join_contact_info(input_data, uid, .config, .config$parquet$encrypt)
    }
  }

  save_to_db <- .config$validation$save_to_db
  if(is.null(save_to_db)) save_to_db <- TRUE

  if(save_to_db) {
    save_rcbms_logs(output, input_data, .cv_ref, .config, .section_ref)
  }

  return(output)
}
