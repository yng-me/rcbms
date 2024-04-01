#' Create case ID
#'
#' @param .data
#' @param .filter_completed
#' @param .config
#' @param .input_data
#'
#' @return
#' @export
#'
#' @examples
create_case_id <- function(
    .data,
    .filter_completed = TRUE,
    .config = getOption("rcbms.config"),
    .input_data = NULL) {
  if (is.null(.input_data)) {
    if (exists("current_input_data")) {
      .input_data <- current_input_data
    }
  }

  .data <- collect_first(.data)

  if (.input_data == "bp") {
    return(.data)
  }

  if (!("case_id" %in% names(.data))) {
    if (.input_data == "ilq") {
      .data <- .data |>
        dplyr::mutate(
          case_id = paste0(region_code, province_code, city_mun_code, barangay_code, ean, bsn, isn),
          .before = 1
        )
    } else if (.input_data == "hp") {
      .data <- .data |>
        dplyr::mutate(
          case_id = paste0(region_code, province_code, city_mun_code, barangay_code, ean, bsn, husn, hsn),
          .before = 1
        )
    }
  }

  if (!is.null(.config$completed_cases) & .filter_completed) {
    .data <- .data |>
      dplyr::filter(case_id %in% .config$completed_cases)
  }

  attr(.data$case_id, "label") <- "Case ID"

  return(.data)
}

#' Create line number ID
#'
#' @param .data
#' @param .join_with
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
create_line_number_id <- function(.data, .join_with = NULL, ...) {
  .data <- collect_first(.data)

  if (!("line_number" %in% names(.data))) {
    stop("Line number variable is not present in the data frame.")
  }

  if (!("case_id" %in% names(.data))) {
    .data <- .data |> create_case_id(...)
  }

  .data <- .data |>
    dplyr::mutate(
      line_number_id = dplyr::if_else(
        !is.na(line_number),
        paste0(
          case_id,
          stringr::str_pad(as.integer(line_number), width = 2, pad = "0")
        ),
        NA_character_
      ),
      .after = 1
    )

  if (!is.null(.join_with)) {
    .data <- .data |>
      dplyr::left_join(.join_with, by = "line_number_id")
  }

  attr(.data$line_number_id, "label") <- "Line number ID"
  return(.data)
}


#' Create barangay geo ID
#'
#' @param .data
#' @param .config
#'
#' @return
#' @export
#'
#' @examples
create_barangay_geo <- function(.data, .config = getOption("rcbms.config")) {
  if (!("barangay_geo" %in% names(.data))) {
    .data <- .data |>
      collect_first() |>
      dplyr::mutate(barangay_geo = paste0(region_code, province_code, city_mun_code, barangay_code))
  }

  attr(.data$barangay_geo, "label") <- "Barangay geo ID"
  return(.data)
}


#' Create unique ID sequentially
#'
#' @param .format
#'
#' @return
#' @export
#'
#' @examples
create_uid <- function(.format) {
  concat <- c()
  for (i in 1:nrow(.format)) {
    append <- ""
    if (i < nrow(.format)) append <- ", "
    concat <- paste0(
      concat,
      paste0(
        "sprintf('%0",
        .format$digit[i],
        "d', as.integer(",
        .format$var_name[i],
        "))",
        append
      )
    )
  }

  return(paste0("paste0(", concat, ")"))
}


#' Title
#'
#' @param .data
#'
#' @return
#' @export
#'
#' @examples
add_uuid <- function(.data, .id_name = "uuid") {
  uid <- uuid::UUIDgenerate(n = nrow(.data))
  .data |>
    tibble::add_column(!!as.name(.id_name) := uid, .before = 1)
}


collect_first <- function(.data) {
  if ("ArrowObject" %in% class(.data) || "arrow_dplyr_query" %in% class(.data)) {
    .data <- .data |> dplyr::collect()
  }
  return(.data)
}
