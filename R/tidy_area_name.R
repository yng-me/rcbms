#' Tidy area name
#'
#' @param .area_name
#'
#' @return
#' @export
#'
#' @examples
#'

tidy_area_name <- function(.area_name, .add_length) {

  if(.add_length == 1) {
    barangay_geo_var <- 'barangay_geo_new'
    over_all_code <- "0000000000"
  } else {
    barangay_geo_var <- "barangay_geo"
    over_all_code <- "000000000"
  }
  .area_name |>
    dplyr::select(
      area_code = !!as.name(barangay_geo_var),
      area_name = barangay,
    ) |>
    dplyr::mutate(level = 4) |>
    dplyr::bind_rows(
      .area_name |>
        dplyr::transmute(
          area_code = stringr::str_pad(
            stringr::str_sub(!!as.name(barangay_geo_var), 1, 6 + .add_length),
            width = 9 + .add_length,
            pad = "0",
            side = "right"
          ),
          area_name = city_mun
        ) |>
        dplyr::mutate(level = 3) |>
        dplyr::distinct(.keep_all = T)
    ) |>
    dplyr::bind_rows(
      .area_name |>
        dplyr::transmute(
          area_code = stringr::str_pad(
            stringr::str_sub(!!as.name(barangay_geo_var), 1, 4 + .add_length),
            width = 9 + .add_length,
            pad = "0",
            side = "right"
          ),
          area_name = province
        ) |>
        dplyr::mutate(level = 2) |>
        dplyr::distinct(.keep_all = T)
    ) |>
    dplyr::bind_rows(
      transform_area_name(.area_name, .add_length = .add_length) |>
        dplyr::transmute(
          area_code = stringr::str_pad(
            region_code,
            width = 9 + .add_length,
            pad = "0",
            side = "right"
          ),
          area_name = region
        ) |>
        dplyr::mutate(level = 1) |>
        dplyr::distinct(.keep_all = T)
    ) |>
    dplyr::add_row(
      tibble::tibble(
        area_code = over_all_code,
        area_name = "",
        level = 0
      )
    )
}
