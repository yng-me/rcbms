#' Title
#'
#' @param .data
#' @param .references
#' @param .config
#'
#' @return
#' @export
#'
#' @examples
select_and_sort_columns <- function(
    .data,
    .references = get_config("references"),
    .config = getOption("rcbms.config")) {
  df <- .data |>
    dplyr::select(
      dplyr::any_of(
        c(
          "case_id",
          "region_code",
          "province_code",
          "city_mun_code",
          "barangay_code",
          "ean",
          "bsn",
          "husn",
          "hsn",
          "line_number"
        )
      ),
      order(names(.data))
    )

  if (tolower(.config$mode$type) == "portal") {
    included <- .references$data_dictionary |>
      dplyr::filter(is_included_for_portal == 1) |>
      dplyr::collect() |>
      dplyr::pull(variable_name_new)

    df <- df |>
      dplyr::select(dplyr::any_of(included)) |>
      add_uuid() |>
      normalize_variable()
  }

  return(df)
}
