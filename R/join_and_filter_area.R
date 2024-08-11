#' Title
#'
#' @param .data
#' @param ...
#' @param .aggregation
#' @param .config
#' @param .input_data
#' @param .retain_agg_cols
#' @param .minimized
#'
#' @return
#' @export
#'
#' @examples
join_and_filter_area <- function(
    .data,
    ...,
    .aggregation = get_config("aggregation"),
    .config = getOption("rcbms.config"),
    .input_data = NULL,
    .retain_agg_cols = FALSE,
    .minimized = TRUE) {
  if (is.null(.input_data)) {
    if (exists("CURRENT_INPUT_DATA")) {
      .input_data <- CURRENT_INPUT_DATA
    } else {
      .input_data <- "hp"
    }
  }

  if (!("case_id" %in% names(.data)) & .input_data == "hp") {
    .data <- .data |> create_case_id()
  }

  if (exists("current_area_code")) {
    current_area <- current_area_code
  } else {
    current_area <- NULL
  }

  .data <- .data |>
    join_area(.aggregation[[.input_data]]) |>
    filter_area(.aggregation[[.input_data]], .config, current_area)

  if (!.retain_agg_cols) {
    .data <- .data |> dplyr::select(-dplyr::ends_with("_agg"))
  }

  if (.minimized) {
    .data <- .data |>
      dplyr::select(
        -dplyr::any_of(c("funding_source", "psu_weight", "sample_psu")),
        -dplyr::ends_with("_popn"),
        -dplyr::ends_with("_geo")
      )
  }

  attr(.data$aggregate_level, "label") <- "Aggregate level"

  .data |>
    dplyr::select(
      dplyr::any_of("case_id"),
      dplyr::starts_with("region"),
      dplyr::starts_with("province"),
      dplyr::starts_with("city_mun"),
      dplyr::starts_with("barangay"),
      dplyr::any_of(c("aggregate_level", "is_huc")),
      dplyr::everything()
    )
}


join_area <- function(.data, .aggregation) {
  area_name <- .aggregation$areas_all |>
    dplyr::select(-dplyr::ends_with("code"))

  .data <- .data |>
    create_barangay_geo() |>
    dplyr::left_join(area_name, by = "barangay_geo", multiple = "first") |>
    dplyr::mutate(aggregate_level = !!as.name(paste0(.aggregation$value, "_agg"))) |>
    dplyr::filter(!is.na(aggregate_level))
}


filter_area <- function(
    .data,
    .aggregation,
    .config,
    .current_area = NULL) {
  if (.config$aggregation$level < length(.aggregation$levels) && !is.null(.current_area)) {
    current_area_filter <- paste0(.aggregation$levels[.config$aggregation$level + 1], "_geo")
    .data <- .data |>
      dplyr::filter(!!as.name(current_area_filter) == .current_area)
  }

  return(.data)
}
