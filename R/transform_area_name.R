#' Transform area name
#'
#' @param .area_name
#' @param .add_length
#'
#' @return
#' @export
#'
#' @examples
#'

transform_area_name <- function(.area_name, .add_length = 0) {

  if(.add_length == 0) {

    df <- .area_name |>
      dplyr::collect() |>
      dplyr::mutate(add_length = .add_length) |>
      dplyr::mutate(
        is_huc = as.integer(is_huc),
        barangay_geo = dplyr::if_else(
          add_length == 1,
          stringr::str_pad(barangay_geo_new, width = 10, pad = "0"),
          stringr::str_pad(barangay_geo, width = 9, pad = "0")
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
      dplyr::mutate(
        region_agg = region,
        province_agg = dplyr::if_else(
          is_huc == 1,
          paste0(city_mun, ", ", region),
          paste0(province, ", ", region)
        ),
        city_mun_agg = dplyr::if_else(
          is_huc == 1,
          city_mun,
          paste0(city_mun, ", ", province)
        ),
        barangay_agg = barangay
      ) |>
      dplyr::select(
        dplyr::starts_with("region"),
        dplyr::starts_with("province"),
        dplyr::starts_with("city_mun"),
        dplyr::starts_with("barangay"),
        dplyr::everything()
      ) |>
      dplyr::select(
        dplyr::ends_with("_code"),
        dplyr::ends_with("_geo"),
        dplyr::ends_with("_agg"),
        dplyr::everything()
      ) |>
      dplyr::select(-add_length, -barangay_geo_new)

    attr(df$region, 'label') <- 'Region'
    attr(df$is_huc, 'label') <- 'Highly Ubranized City (HUC)'
    attr(df$province, 'label') <- 'Province'
    attr(df$city_mun, 'label') <- 'City/Municipality'
    attr(df$barangay, 'label') <- 'Barangay'

    return(df)

  } else if (.add_length == 1) {

    df <- .area_name |>
      dplyr::mutate(
        barangay_geo = paste0(
          region_code,
          province_code,
          city_mun_code,
          barangay_code
        )
      ) |>
      dplyr::select(
        barangay_geo,
        region,
        province,
        city_mun,
        barangay,
        total_population,
        total_hh,
        average_hh_size
      ) |>
      dplyr::group_by(
        barangay_geo,
        region,
        province,
        city_mun,
        barangay
      ) |>
      dplyr::summarise(
        total_population = sum(total_population, na.rm = T),
        total_hh = sum(total_hh, na.rm = T),
        average_hh_size = mean(average_hh_size, na.rm = T),
        .groups = 'drop'
      )
  } else {
    stop('Invalid add_length argument.')
  }

  return(df)
}
