#' Generate validation output in parallel mode
#'
#' @param .cv
#' @param .cv_ref
#' @param .config
#'
#' @return
#' @export
#'
#' @examples

generate_validation_par <- function(.cv, .cv_ref, .config) {

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

  add_info <- .config$validation$include_additional_info
  uid <- .config$project[[CURRENT_INPUT_DATA]]$id
  if (is.null(uid)) uid <- "case_id"

  # =============================================
  parallel::detectCores()
  n_cores <- parallel::detectCores() - 2
  my_cluster <- parallel::makeCluster(n_cores, type = "PSOCK")
  doParallel::registerDoParallel(cl = my_cluster)

  `%dopar%` <- foreach::`%dopar%`

  output <- foreach::foreach(i = seq_along(result_names)) %dopar% {

    result_name <- result_names[i]

    if(.config$verbose) {
      cat(paste0(str_pad(formatC(nrow(.cv[[result_name]]), big.mark = ','), width = 7), ': ', result_name, '\n'))
    }

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

    if(add_info & 'contact__' %in% names(output_temp)) {
      group_cols <- c(group_cols, 'contact__')
    }

    output_temp |>
      dplyr::select(-dplyr::any_of(c("region", "province", "city_mun", "barangay", "ean"))) |>
      dplyr::group_by(dplyr::pick(dplyr::any_of(group_cols))) |>
      tidyr::nest(.key = "info") |>
      dplyr::mutate(info = purrr::map_chr(info, \(x) {
        x |>
          dplyr::mutate_all(as.character) |>
          dplyr::mutate_all(~ dplyr::if_else(is.na(.), "Missing/NA", .)) |>
          jsonlite::toJSON() |>
          as.character() |>
          encrypt_info()
      })) |>
      dplyr::ungroup() |>
      dplyr::tibble()
  }

  parallel::stopCluster(cl = my_cluster)
  output <- dplyr::bind_rows(output)

  if(add_info & 'contact__' %in% names(output)) {
    output <- output |>
      dplyr::rename(contact = contact__)
  }

  save_rcbms_logs(output, input_data, .cv_ref, .config)

  return(output)
}
