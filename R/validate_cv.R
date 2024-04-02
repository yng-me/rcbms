#' Title
#'
#' @param .data
#' @param ...
#' @param .id
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

select_cv <- function(.data, ...) {

  .data <- .data |>
    dplyr::collect() |>
    validate_select(...)

  set_class(.data, "rcbms_cv_tbl")
}


validate_select <- function(.data, ...) {

  if(!exists("config")) {
    config <- getOption('rcbms.config')
  } else if(exists("config") & !inherits(config, "rcbms_config")) {
    config <- getOption('rcbms.config')
  }

  uid <- config$project[[current_input_data]]$id

  if (is.null(uid)) uid <- "case_id"

  if (!exists("current_input_data")) {
    current_input_data <- config$input_data[1]
  }

  add_length <- config$project$add_length

  geo_cols_name <- c('region', 'province', 'city_mun', 'barangay')
  geo_cols <- paste0(geo_cols_name, '_code')

  if (length(which(geo_cols %in% names(.data))) == 4) {
    .data <- .data |> create_barangay_geo()
  }

  if (!("barangay_geo" %in% names(.data)) & current_input_data %in% c("hp", "ilq")) {
    .data <- .data |>
      dplyr::mutate(barangay_geo = stringr::str_sub(case_id, 1, 9 + add_length))
  }

  if (current_input_data %in% c("hp", "ilq")) {
    .data <- .data |>
      dplyr::mutate(
        ean = stringr::str_sub(case_id, 10 + add_length, 15 + add_length)
      )
  }

  if (!("line_number" %in% names(.data)) & current_input_data %in% c("hp", "ilq")) {
    .data <- .data |>
      dplyr::mutate(line_number = NA_character_)
  }


  if ("barangay_geo" %in% names(.data)) {
    .data <- .data |>
      dplyr::select(
        dplyr::any_of(c(uid, geo_cols_name, "ean", "line_number")),
        ...
      )
  }

  join_hh_info <- config$validation$include_additional_info

  summary_record <- config$project[[current_input_data]][['summary_record']]

  if (isTRUE(join_hh_info) & !is.null(summary_record)) {
    parquet <- get_config("parquet")
    summary_df <- parquet[[current_input_data]][[summary_record]]
    if (!is.null(summary_df)) {
      hh_info <- summary_df |>
        dplyr::collect() |>
        create_case_id(.input_data = current_input_data) |>
        dplyr::select(
          dplyr::any_of(
            c(
              uid,
              "hh_head",
              "respondent_contact_number",
              "contact_number",
              "email_add",
              "address",
              "floor_number",
              "subdivision_or_village",
              "sitio_or_purok"
            )
          )
        )

      .data <- .data |>
        dplyr::left_join(hh_info |> dplyr::collect(), by = uid)
    }
  }

  .data |> dplyr::select(
    dplyr::any_of(uid),
    dplyr::matches(paste0("^", geo_cols_name)),
    dplyr::any_of(c("ean", "line_number")),
    ...
  )
}
