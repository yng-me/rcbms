#' Generate validation output
#'
#' @param .cv
#' @param .config
#' @param .refs
#'
#' @return
#' @export
#'
#' @examples
#'

generate_validation <- function(.cv, .config, .refs) {

  input_data <- .config$input_data[1]

  priority_levels <- tolower(stringr::str_trim((.config$validation$priority_level)))
  .refs$ref_cv <- dplyr::filter(.refs$ref_cv, tolower(priority_level) %in% priority_levels)

  result_names <- names(.cv)
  result_names <- result_names[result_names %in% .refs$ref_cv$validation_id]

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
  add_contact_info <- .config$validation$include_contact_info
  if(is.null(add_contact_info)) { add_contact_info <- TRUE }

  uid <- .config$project[[input_data]]$id
  if (is.null(uid)) uid <- "case_id"

  output_list <- list()

  for (i in seq_along(result_names)) {

    result_name <- result_names[i]

    if(.config$verbose) {
      cat(paste0(str_pad(formatC(nrow(.cv[[result_name]]), big.mark = ','), width = 7), ': ', result_name, '\n'))
    }

    n_incons <- formatC(nrow(.cv[[result_name]]), big.mark = ',')
    cli::cli_text('{i} of {length(result_names)}: {result_name} ({n_incons})\n')

    output_temp <- .cv[[result_name]] |>
      dplyr::mutate(mode_id = result_name) |>
      dplyr::rename(input_data_id = !!as.name(uid))

    if (!("line_number" %in% names(output_temp))) {
      output_temp <- output_temp |>
        dplyr::mutate(line_number = '00')
    }

    output_temp <- output_temp |>
      dplyr::mutate(
        line_number = dplyr::if_else(is.na(line_number), '00', line_number)
      )

    if(add_info) {
      output_list[[result_name]] <- output_temp |>
        dplyr::select(-dplyr::any_of(c("region", "province", "city_mun", "barangay", "ean"))) |>
        dplyr::group_by(mode_id, input_data_id, line_number) |>
        tidyr::nest(.key = "info") |>
        dplyr::ungroup() |>
        dplyr::tibble()

    } else {
      output_list[[result_name]] <- output_temp |>
        dplyr::select(mode_id, input_data_id, line_number) |>
        dplyr::tibble()
    }
  }

  if(length(output_list) > 0) {

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
      output <- dplyr::bind_rows(output_list)
    }

    if(add_contact_info) {
      output <- join_contact_info(output, input_data, uid, .config, .config$parquet$encrypt)
    }
  }

  save_to_db <- .config$validation$save_to_db
  if(is.null(save_to_db)) save_to_db <- TRUE

  if(save_to_db) {

    log_id <- save_rcbms_log(
      .data = output,
      .config = .config,
      .metadata = get_cv_metadata(output, .config, .refs)
    )

    if(is.null(output)) output <- list()
    attr(output, 'rcbms_log_id') <- paste0('rcbms_log_id: ', log_id)
  }

  return(output)

}



get_cv_metadata <- function(.output, .config, .refs) {

  current_time <- stringr::str_sub(as.POSIXct(Sys.time()) - (3600 * 8), 1, 19)

  values <- list(
    area_code = .refs$ref_current_area,
    total_cases = 0L,
    total_cases_unique = 0L,
    total_priority_a = 0L,
    total_priority_b = 0L,
    total_priority_c = 0L,
    total_priority_d = 0L,
    partial = 0L,
    status = 2L,
    category = NA_character_,
    verified_at = current_time,
    validated_at = current_time
  )

  if(!is.null(.output)) {

    priority_ref <- .refs$ref_cv |>
      dplyr::select(
        mode_id = validation_id,
        dplyr::any_of(c('priority_level', 'section'))
      )

    if(!("priority_level" %in% names(priority_ref))) {
      priority_ref <- priority_ref |>
        dplyr::mutate(priority_level = NA_character_)
    }

    if(!("section" %in% names(priority_ref))) {
      priority_ref <- priority_ref |>
        dplyr::mutate(section = NA_character_)
    }

    priority_df <- .output |>
      dplyr::select(mode_id) |>
      dplyr::left_join(priority_ref, by = "mode_id") |>
      dplyr::mutate(priority_level = stringr::str_trim(tolower(priority_level)))

    get_priority <- function(.level) {
      priority_df |>
        dplyr::filter(priority_level == .level) |>
        nrow()
    }

    values$status = 0L
    values$total_priority_a <- get_priority("a")
    values$total_priority_b <- get_priority("b")
    values$total_priority_c <- get_priority("c")
    values$total_priority_d <- get_priority("d")
    values$total_cases <- nrow(.output)
    values$total_cases_unique <- .output |>
      dplyr::distinct(input_data_id) |>
      nrow()

    values$verified_at <- NA_character_
    values$validated_at <- NA_character_

    values$summary_info <- list(
      priority_level = priority_df |>
        dplyr::count(priority_level, name = 'value') |>
        dplyr::rename(key = priority_level),
      section = priority_df |>
        dplyr::count(section, name = 'value') |>
        dplyr::rename(key = section)
    )

  }

  if(!is.null(.refs$ref_section)) {

    selected_sec <- .refs$ref_section |>
      dplyr::filter(included, builtin_included) |>
      nrow()

    if(selected_sec < nrow(.refs$ref_section) & .config$mode$edit %in% c(1, 2, 5)) {
      values$partial <- 1L
    }
  }

  values

}

