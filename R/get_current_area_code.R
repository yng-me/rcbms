#' Title
#'
#' @param .data
#' @param .input_data
#' @param .config
#'
#' @return
#' @export
#'
#' @examples
#'

get_current_area_code <- function(.parquet, .input_data, .config) {

  current_area_code <- NULL
  area_code_vars <- paste0(c('region', 'province', 'city_mun', 'barangay', 'ean'), "_geo")

  agg_record <- .config$project[[.input_data]][['summary_record']]

  add_length <- .config$project$add_length
  agg_level <- .config$aggregation$level
  if(.input_data == 'bp' & agg_level > 4) {
    agg_level <- 4
  }

  uid <- .config$project[[.input_data]]$id

  agg_data <- .parquet[[.input_data]][[agg_record]] |>
    dplyr::collect() |>
    dplyr::mutate(
      area_code = !!as.name(uid),
    ) |>
    dplyr::mutate(
      region_geo = stringr::str_sub(area_code, 1, 2),
      province_geo = stringr::str_sub(area_code, 1, 4 + add_length),
      city_mun_geo = stringr::str_sub(area_code, 1, 6 + add_length),
      barangay_geo = stringr::str_sub(area_code, 1, 9 + add_length)
    )

  if(.input_data != 'bp') {
    agg_data <- agg_data |>
      dplyr::mutate(
        ean_geo = stringr::str_sub(area_code, 1, 15 + add_length)
      )

    current_area_code <- agg_data |>
      dplyr::distinct(ean_geo) |>
      dplyr::pull()
  }


  rev_area <- rev(area_code_vars)[2:5]

  for (i in seq_along(rev_area)) {
    if(length(current_area_code) != 1) {
      current_area_code <- agg_data |>
        dplyr::transmute(code = !!as.name(rev_area[i])) |>
        dplyr::distinct(code) |>
        dplyr::pull()
    }
  }

  if(length(current_area_code) > 1) {
    current_area_code <- 'all'
  }

  return(current_area_code)

}
