#' Title
#'
#' @param .references
#'
#' @return
#' @export
#'
#' @examples
#'

tidy_area_name <- function(.references) {
  .references$area_name |>
    select(
      area_code = barangay_geo,
      area_name = barangay,
    ) |>
    mutate(level = 4) |>
    bind_rows(
      .references$area_name |>
        transmute(
          area_code = str_pad(str_sub(barangay_geo, 1, 6), width = 9, pad = '0', side = 'right'),
          area_name = city_mun
        ) |>
        mutate(level = 3) |>
        distinct(.keep_all = T)
    ) |>
    bind_rows(
      .references$area_name |>
        transmute(
          area_code = str_pad(str_sub(barangay_geo, 1, 4), width = 9, pad = '0', side = 'right'),
          area_name = province
        ) |>
        mutate(level = 2) |>
        distinct(.keep_all = T)
    ) |>
    bind_rows(
      transform_area_name(.references) |>
        transmute(
          area_code = str_pad(region_code, width = 9, pad = '0', side = 'right'),
          area_name = region
        ) |>
        mutate(level = 1) |>
        distinct(.keep_all = T)
    ) |>
    add_row(tibble(
      area_code = "000000000",
      area_name = "",
      level = 0
    ))
}
