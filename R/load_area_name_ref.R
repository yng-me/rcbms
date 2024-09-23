#' Load area name references
#'
#' @param .gid
#'
#' @return
#' @export
#'
#' @examples
#'

load_area_name_refs <- function(.gid) {

  required_cols <- c(
    'region',
    'province',
    'city_mun',
    'barangay',
    'barangay_geo_new',
    'barangay_geo',
    'income_class',
    'is_huc',
    'class',
    '2015_popn',
    '2020_popn',
    'funding_source'
  )

  load_refs_from_gsheet(.gid, required_cols, col_types = 'cccciiciciii') |>
    dplyr::mutate(
      barangay_geo_new = stringr::str_pad(
        stringr::str_extract(barangay_geo_new, '\\d+'),
        width = 10,
        pad = '0'
      ),
      barangay_geo = stringr::str_pad(
        stringr::str_extract(barangay_geo, '\\d+'),
        width = 9,
        pad = '0'
      )
    )

}


#' Load new area name reference
#'
#' @param .gid
#'
#' @return
#' @export
#'
#' @examples
#'
load_area_name_new_refs <- function(.gid) {

  required_cols <- c(
    'region_code',
    'province_code',
    'city_mun_code',
    'barangay_code',
    'ean',
    'barangay',
    'average_hh_size',
    'total_population',
    'total_hh',
    'total_workload',
    'survey_indicator',
    'implementation'
  )

  required_cols_reg <- c(
    'region_code',
    'region',
    'region_full_name'
  )

  required_cols_prov <- c(
    'province_code',
    'province'
  )

  required_cols_city_mun <- c(
    'province_code',
    'city_mun_code',
    'city_mun'
  )

  load_refs_from_gsheet(.gid, required_cols, col_types = 'ccccccdiiiii', .sheet_name = 'area_name') |>
    dplyr::left_join(
      load_refs_from_gsheet(.gid, required_cols_reg, col_types = 'ccc', .sheet_name = 'region_name'),
      by = 'region_code'
    ) |>
    dplyr::left_join(
      load_refs_from_gsheet(.gid, required_cols_prov, col_types = 'cc', .sheet_name = 'province_name'),
      by = 'province_code'
    ) |>
    dplyr::left_join(
      load_refs_from_gsheet(.gid, required_cols_city_mun, col_types = 'ccc', .sheet_name = 'city_mun_name'),
      by = c('province_code', 'city_mun_code')
    ) |>
    dplyr::select(
      region_code,
      province_code,
      city_mun_code,
      barangay_code,
      ean,
      region_full_name,
      region,
      province,
      city_mun,
      barangay,
      average_hh_size,
      total_population,
      total_hh,
      total_workload,
      survey_indicator,
      implementation
    )
}
