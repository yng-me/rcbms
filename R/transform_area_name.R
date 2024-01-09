#' Title
#'
#' @param .references
#' @param .add_length
#'
#' @return
#' @export
#'
#' @examples
#'

transform_area_name <- function(.references, .add_length = 0) {

  regions <- .references$valueset |>
    dplyr::filter(name == 'area_name_region') |>
    dplyr::collect() |>
    dplyr::transmute(
      region_code = stringr::str_pad(as.integer(value), width = 2, pad = '0'),
      region = label
    )

  regions_long <- .references$valueset |>
    dplyr::filter(name == 'area_name_region_long') |>
    dplyr::collect() |>
    dplyr::transmute(
      region_code = stringr::str_pad(as.integer(value), width = 2, pad = '0'),
      region_long = label
    )

  .data <- .references$area_name |>
    dplyr::collect() |>
    dplyr::mutate(add_length = .add_length) |>
    dplyr::mutate(
      barangay_geo = dplyr::if_else(
        add_length == 1,
        stringr::str_pad(barangay_geo_new, width = 10, pad = '0'),
        stringr::str_pad(barangay_geo, width = 9, pad = '0')
      )
    ) |>
    dplyr::mutate(
      region_code = stringr::str_sub(barangay_geo, 1, 2),
      province_code = stringr::str_sub(barangay_geo, 3, 4 + add_length),
      city_mun_code = stringr::str_sub(barangay_geo, 5 + add_length, 6 + add_length),
      barangay_code = stringr::str_sub(barangay_geo, 7 + add_length, 9 + add_length),
      province_geo = stringr::str_sub(barangay_geo, 1, 4 + add_length),
      city_mun_geo = stringr::str_sub(barangay_geo, 1, 6 + add_length)
    ) |>
    dplyr::left_join(regions, by = 'region_code', multiple = 'first') |>
    dplyr::left_join(regions_long, by = 'region_code', multiple = 'first') |>
    dplyr::mutate(
      region_agg = region,
      province_agg = dplyr::if_else(
        is_huc == 1,
        paste0(city_mun, ', ', region),
        paste0(province, ', ', region)
      ),
      city_mun_agg = dplyr::if_else(
        is_huc == 1,
        city_mun,
        paste0(city_mun, ', ', province)
      ),
      barangay_agg = barangay
    ) |>
    dplyr::select(
      dplyr::starts_with('region'),
      dplyr::starts_with('province'),
      dplyr::starts_with('city_mun'),
      dplyr::starts_with('barangay'),
      dplyr::everything()
    ) |>
    dplyr::select(
      dplyr::ends_with('_code'),
      dplyr::ends_with('_geo'),
      dplyr::ends_with('_agg'),
      dplyr::everything()
    ) |>
    dplyr::select(-add_length, -barangay_geo_new)

  attr(.data$region, 'label') <- 'Region'
  attr(.data$region_long, 'label') <- 'Region (Long Name)'
  attr(.data$is_huc, 'label') <- 'Highly Ubranized City (HUC)'
  attr(.data$province, 'label') <- 'Province'
  attr(.data$city_mun, 'label') <- 'City/Municipality'
  attr(.data$barangay, 'label') <- 'Barangay'

  return(.data)
}
