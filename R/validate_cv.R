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

  uid <- config$project[[CURRENT_INPUT_DATA]]$id

  if (is.null(uid)) uid <- "case_id"

  if (!exists("CURRENT_INPUT_DATA")) {
    CURRENT_INPUT_DATA <- config$input_data[1]
  }

  add_length <- config$project$add_length

  geo_cols_name <- c('region', 'province', 'city_mun', 'barangay')
  geo_cols <- paste0(geo_cols_name, '_code')

  if (length(which(geo_cols %in% names(.data))) == 4) {
    .data <- .data |> create_barangay_geo()
  }

  if (!("barangay_geo" %in% names(.data)) & CURRENT_INPUT_DATA %in% c("hp", "ilq")) {
    .data <- .data |>
      dplyr::mutate(barangay_geo = stringr::str_sub(case_id, 1, 9 + add_length))
  }

  if (CURRENT_INPUT_DATA %in% c("hp", "ilq")) {
    .data <- .data |>
      dplyr::mutate(
        ean = stringr::str_sub(case_id, 10 + add_length, 15 + add_length)
      )
  }

  if (!("line_number" %in% names(.data)) & CURRENT_INPUT_DATA %in% c("hp", "ilq")) {
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

  include_additional_info <- config$validation$include_additional_info
  summary_record <- config$project[[CURRENT_INPUT_DATA]][['summary_record']]
  contact_info_vars <- config$project[[CURRENT_INPUT_DATA]]$variable$contact

  if (
    include_additional_info &
    exists('parquet') &
    !is.null(summary_record) &
    !is.null(contact_info_vars)
  ) {

    summary_df <- parquet[[CURRENT_INPUT_DATA]][[summary_record]]

    if (!is.null(summary_df)) {

      contact_info <- summary_df |>
        dplyr::select(dplyr::any_of(c(uid, contact_info_vars))) |>
        dplyr::collect() |>
        dplyr::group_by(!!as.name(uid))

      contact_info_names <- names(contact_info)
      contact_info_names <- contact_info_names[contact_info_names != uid]


      for(i in seq_along(contact_info_names)) {
        info_name <- contact_info_names[i]

        info_label <- attributes(contact_info[[info_name]])$label

        if(!is.null(info_label)) {

          contact_info <- contact_info |>
            dplyr::rename(!!as.name(info_label) := !!as.name(info_name))
        }
      }

      if(config$validation$stringify_info) {

        contact_info <- contact_info |>
          tidyr::nest(.key = 'contact__') |>
          dplyr::mutate(
            `contact__` = purrr::map_chr(`contact__`, \(x) {
              x |>
                dplyr::mutate_all(as.character) |>
                jsonlite::toJSON() |>
                as.character() |>
                encrypt_info()
            })
          )
      }  else {
        contact_info <- contact_info |>
          tidyr::nest(.key = 'contact__')
      }

      .data <- .data |>
        dplyr::left_join(
          contact_info,
          by = uid
        )
    }
  }

  .data |> dplyr::select(
    dplyr::any_of(uid),
    dplyr::matches(paste0("^", geo_cols_name)),
    dplyr::any_of(c("ean", "line_number")),
    ...,
    dplyr::any_of('contact__')
  )
}
