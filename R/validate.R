#' Title
#'
#' @param .data
#' @param ...
#' @param .id
#' @param .references
#' @param .title
#' @param .description
#' @param .section
#' @param .priority_level
#' @param .primary_data_item
#' @param .validation_type
#' @param .status
#' @param .date_introduced
#'
#' @return
#' @export
#'
#' @examples
#'

validate <- function(
  .data,
  ...,
  .id = NULL,
  .references = get_config("references"),
  .title = get_from_validation_id(.id, .references, "title"),
  .description = get_from_validation_id(.id, .references, "description"),
  .section = get_from_validation_id(.id, .references, "section"),
  .priority_level = get_from_validation_id(.id, .references, "priority_level"),
  .primary_data_item = get_from_validation_id(.id, .references, "primary_data_item"),
  .validation_type = get_from_validation_id(.id, .references, "validation_type"),
  .status = get_from_validation_id(.id, .references, "status"),
  .date_introduced = get_from_validation_id(.id, .references, "date_introduced")
) {

  attr(.data, "validation_id") <- .id
  attr(.data, "title") <- .title
  attr(.data, "description") <- .description
  attr(.data, "section") <- .section
  attr(.data, "priority_level") <- .priority_level
  attr(.data, "primary_data_item") <- .primary_data_item
  attr(.data, "status") <- .status
  attr(.data, "date_introduced") <- .date_introduced

  if(is.null(.validation_type)) .validation_type <- NULL
  if(!(.validation_type %in% c(NA, 1:9))) { .validation_type <- 9 }
  attr(.data, "validation_type") <- as.integer(.validation_type)

  set_class(.data, 'rcbms_cv_tbl')

  .data |> validate_select(...)

}


get_from_validation_id <- function(.validation_id, .references, .info) {

  if(is.null(.validation_id)) return(NULL)
  if(is.null(.references)) return(NA_character_)

  df <- .references$validation |>
    dplyr::collect() |>
    dplyr::filter(validation_id == .validation_id)

  if(nrow(df) == 0) return(NA_character_)

  return(df[[.info]][1])
}


validate_select <- function(.data, ...) {

  config <- getOption('rcbms.config')
  if(!exists("current_input_data")) {
    current_input_data <- config$input_data[1]
  }
  references <- get_config("references")
  add_length <- config$project$add_length

  .data <- .data |>
    dplyr::mutate(
      ean = stringr::str_sub(case_id, 10 + add_length, 15 + add_length),
      barangay_geo = stringr::str_sub(case_id, 1, 9 + add_length)
    )

  if(!('line_number' %in% names(.data)) && current_input_data == "hp") {
    .data <- .data |>
      dplyr::mutate(line_number = NA_character_)
  }

  area_name <- transform_area_name(references, add_length)

  .data <- .data |>
    dplyr::select(-dplyr::any_of(c('region', 'province', 'city_mun', 'barangay'))) |>
    dplyr::left_join(area_name, by = 'barangay_geo', multiple = 'first') |>
    dplyr::select(
      dplyr::any_of(
        c(
          "case_id",
          "region",
          "province",
          "city_mun",
          "barangay",
          "ean",
          "line_number"
        )
      ),
      ...
    )

  join_hh_info <- config$validation$include_additional_info
  summary_record <- get_summary_record(current_input_data)

  if(isTRUE(join_hh_info) & !is.null(summary_record)) {

    uid <- config$project[[current_input_data]]$id
    parquet <- get_config("parquet")
    summary_df <- parquet[[current_input_data]][[summary_record]]
    if(!is.null(summary_df)) {

      hh_info <- summary_df |>
        dplyr::collect() |>
        create_case_id(.input_data = current_input_data) |>
        dplyr::select(
          dplyr::any_of(
            c(
              config$project[[current_input_data]]$id,
              'hh_head',
              'respondent_contact_number',
              'contact_number',
              'email_add',
              'address',
              'floor_number',
              'subdivision_or_village',
              'sitio_or_purok'
            )
          )
        )

    if(is.null(uid)) uid <- "case_id"

    .data <- .data |>
      dplyr::left_join(summary_df |> dplyr::collect(), by = uid)

    }
  }

  return(.data)

}
