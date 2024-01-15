#' Generate validation output
#'
#' @param .cv
#' @param .config
#' @param .detailed_output
#' @param .include_additional_info
#' @param .add_uuid
#' @param .save_as_excel
#' @param .save_as_json
#'
#' @return
#' @export
#'
#' @examples
#'
generate_validation <- function(
  .cv,
  .config = getOption('rcbms.config'),
  .references = get_config("references"),
  .detailed_output = FALSE,
  .include_additional_info = FALSE,
  .add_uuid = FALSE,
  .save_as_excel = FALSE,
  .save_as_json = TRUE
) {

  if(exists("current_input_data")) {
    input_data <- current_input_data
  } else {
    input_data <- .config$input_data[1]
  }

  references <- .references$validation |>
    dplyr::filter(
      cbms_round == as.integer(.config$cbms_round),
      input_data == input_data
    ) |>
    dplyr::collect()

  result_names <- names(.cv)
  result_names <- result_names[
    result_names %in% references$validation_id
  ]

  with_incon <- NULL
  is_not_rcbms_cv_tbl <- NULL
  for(i in seq_along(result_names)) {
    result_name <- result_names[i]
    is_rcbms_cv_tbl <- inherits(.cv[[result_name]], "rcbms_cv_tbl")

    if(!is_rcbms_cv_tbl) {
      is_not_rcbms_cv_tbl <- c(is_not_rcbms_cv_tbl, result_name)
    }

    if(nrow(.cv[[result_name]]) > 0 && is_rcbms_cv_tbl) {
      with_incon <- c(with_incon, result_name)
    }
  }

  if(!is.null(is_not_rcbms_cv_tbl)) {
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

  opt <- .config$validation

  if(!is.null(opt)) {
    if(!is.null(opt$detailed_output)) {
      .detailed_output <- rlang::is_true(as.logical(opt$detailed_output))
    }
    if(!is.null(opt$include_additional_info)) {
      .include_additional_info <- rlang::is_true(as.logical(opt$include_additional_info))
    }
    if(!is.null(opt$add_uuid)) {
      .add_uuid <- rlang::is_true(as.logical(opt$add_uuid))
    }
    if(!is.null(opt$save_as_excel)) {
      .save_as_excel <- rlang::is_true(as.logical(opt$save_as_excel))
    }
    if(!is.null(opt$save_as_json)) {
      .save_as_json <- rlang::is_true(as.logical(opt$save_as_json))
    }
  }

  additiona_info <- NULL
  if(.include_additional_info) {
    additiona_info <- c(
      'hh_head',
      'respondent_contact_number',
      'contact_number',
      'email_add',
      'address',
      'floor_number',
      'subdivision_or_village',
      'sitio_or_purok'
    )
  }

  uid <- .config$project[[current_input_data]]$id
  if(is.null(uid)) uid <- "case_id"

  for(i in seq_along(result_names)) {

    result_name <- result_names[i]

    output_temp <- .cv[[result_name]] |>
      dplyr::mutate(validation_id = result_name) |>
      dplyr::select(
        validation_id,
        dplyr::any_of(c(uid, "line_number", additiona_info))
      )

    if(!('line_number' %in% names(output_temp)) && input_data == "hp") {
      output_temp <- output_temp |>
        dplyr::mutate(line_number = NA_character_)
    }

    if(i == 1) {
      output <- output_temp |> dplyr::tibble()
    } else {
      output <- output |>
        dplyr::bind_rows(output_temp) |>
        dplyr::tibble()
    }

  }

  if(!is.null(output)) {

    if(.detailed_output) {

      add_length <- .config$project$add_length

      output <- output |>
        dplyr::mutate(
          barangay_geo = stringr::str_sub(case_id, 1, 9 + add_length)
        ) |>
        dplyr::left_join(
          .references$area_name |>
            dplyr::collect() |>
            transform_area_name() |>
            dplyr::select(
              barangay_geo,
              region,
              province,
              city_mun,
              barangay
            ),
          by = 'barangay_geo'
        ) |>
        dplyr::left_join(references, by = 'validation_id') |>
        dplyr::select(-dpylr::any_of("barangay_geo"))
    }

    if(.add_uuid && input_data == "hp") {
      output <- output |> add_uuid(.id_name = "id")
    }

    if(.add_uuid && "uuid" %in% names(output)) {
      output <- output |> dplyr::rename(id = "uuid")
    }

    save_rcbms_logs(output, input_data, .references, .config)

    # if(.save_as_json) {
    #   save_as_json(
    #     .output = output,
    #     .input_data = input_data,
    #     .config = .config
    #   )
    # }

    if(.save_as_excel) {
      # save_as_excel(
      #   .output = output,
      #   .input_data = input_data,
      #   .config = .config,
      #   .detailed = .detailed_output
      # )
    }

  }

  return(output)

}
