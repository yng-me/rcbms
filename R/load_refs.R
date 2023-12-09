#' Title
#'
#' @param .config
#'
#' @return
#' @export
#'
#' @examples
load_refs <- function(.config = getOption('rcbms_config')) {

  if(is.null(.config)) stop('Config not found.')

  refs <- list()

  wd <- .config$working_directory
  if(is.null(wd)) wd <- ''

  wd_project <- create_new_folder(paste0(wd, '/src/', .config$cbms_round, '/references'))
  wd_base_ref <- create_new_folder(paste0(wd, '/references'))

  pq_dcf <- paste0(wd_project, '/ref_data_dictionary.parquet')
  pq_cv <- paste0(wd_project, '/ref_validation.parquet')
  pq_ts <- paste0(wd_project, '/ref_tabulation.parquet')

  pq_vs <- paste0(wd_base_ref, '/ref_valueset.parquet')
  pq_anm <- paste0(wd_base_ref, '/ref_area_name.parquet')

  refs_exist <- file.exists(pq_dcf) &
    file.exists(pq_vs) &
    file.exists(pq_anm) &
    file.exists(pq_cv) &
    file.exists(pq_ts)

  if((!refs_exist | .config$reload_refs) & is_online()) {

    googlesheets4::gs4_deauth()

    arrow::write_parquet(
      suppressWarnings(load_data_dictionary(
        .gid = .config$env$DATA_DICTIONARY,
        .cbms_round = .config$cbms_round
      )),
      pq_dcf
    )

    arrow::write_parquet(
      suppressWarnings(load_valueset(.config$env$VALUESET)),
      pq_vs
    )

    arrow::write_parquet(
      suppressWarnings(load_area_name(.config$env$AREA_NAME)),
      pq_anm
    )

    arrow::write_parquet(
      suppressWarnings(load_validation_refs(.config$env$VALIDATION)),
      pq_cv
    )
    arrow::write_parquet(
      suppressWarnings(load_tabulation_refs(.config$env$TABULATION)),
      pq_ts
    )

  }

  refs$data_dictionary <- arrow::open_dataset(pq_dcf)
  refs$valueset <- arrow::open_dataset(pq_vs)
  refs$validation <- arrow::open_dataset(pq_cv)
  refs$tabulation <- arrow::open_dataset(pq_ts)
  refs$area_name <- arrow::open_dataset(pq_anm)

  return(refs)
}

#' Title
#'
#' @param .gid
#' @param .sheet
#' @param .range
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
fetch_gsheet <- function(.gid, .sheet = NULL, .range = NULL, ...) {
  ss <- paste0("https://docs.google.com/spreadsheets/d/", .gid)
  if(!is.null(.sheet)) .sheet <- as.character(.sheet)

  googlesheets4::read_sheet(
    ss = ss,
    sheet = .sheet,
    range = .range,
    trim_ws = T,
    ...
  ) |> clean_colnames()
}


#' Title
#'
#' @param .data
#' @param .required_cols
#'
#' @return
#' @export
#'
#' @examples
#'
validate_required_cols <- function(.data, .required_cols) {
  required_cols_which <- which(.required_cols %in% names(.data))

  if(length(required_cols_which) < length(.required_cols)) {
    stop('Invalid column names specified.')
  }

  return(.data)
}


#' Title
#'
#' @param .gid
#' @param .required_cols
#' @param .sheet
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
load_refs_from_gsheet <- function(.gid, .required_cols, .sheet = NULL, .start_at = 1, ...) {

  range <- paste0(LETTERS[.start_at], ':', LETTERS[length(.required_cols) + .start_at - 1])
  dd <- fetch_gsheet(.gid, .sheet, .range = range, ...)
  return(validate_required_cols(dd, .required_cols))

}



#' Load data dictionary
#'
#' @param .gid
#' @param .cbms_round
#'
#' @return
#' @export
#'
#' @examples
#'
load_data_dictionary <- function(.gid, .cbms_round = NULL) {

  required_cols <- c(
    'variable_name',
    'variable_name_new',
    'item',
    'sub_item',
    'label',
    'valueset',
    'type',
    'length',
    'is_included',
    'privacy_level'
  )

  load_refs_from_gsheet(.gid, required_cols, .cbms_round, col_types = 'ccccccciii')
}


#' Load area name
#'
#' @param .gid
#'
#' @return
#' @export
#'
#' @examples
#'
load_area_name <- function(.gid) {

  required_cols <- c(
    # 'region',
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

  load_refs_from_gsheet(
    .gid,
    .required_cols = required_cols,
    .start_at = 2,
    col_types = 'ccciiciciii'
  ) |>
  dplyr::mutate(
    barangay_geo_new = stringr::str_pad(stringr::str_extract(barangay_geo_new, '\\d+'), width = 10, pad = '0'),
    barangay_geo = stringr::str_pad(stringr::str_extract(barangay_geo, '\\d+'), width = 9, pad = '0')
  )
}

#' Load valueset
#'
#' @param .gid
#'
#' @return
#' @export
#'
#' @examples
#'
load_valueset <- function(.gid) {
  load_refs_from_gsheet(
    .gid,
    .required_cols = c('name', 'value', 'label'),
    col_types = 'ccc'
  )
}


load_validation_refs <- function(.gid) {
  required_cols <- c(
    'validation_id',
    'title',
    'description',
    'primary_data_item',
    'section',
    'priority_level'
  )
  load_refs_from_gsheet(.gid, required_cols, col_types = 'cccccc')
}

load_tabulation_refs <- function(.gid) {
  required_cols <- c(
    'tabulation_id',
    'tab_name',
    'table_number',
    'title',
    'subtitle',
    'description',
    'is_included',
    'precision',
    'col_decimal_format',
    'col_width_all',
    'col_width_first',
    'col_width_last',
    'row_height_header',
    'row_reset_last'
  )
  load_refs_from_gsheet(.gid, required_cols, col_types = 'cccccciiciiici')
}


#' Title
#'
#' @param .data
#' @param .add_length
#'
#' @return
#' @export
#'
#' @examples
transform_area_name <- function(.data, .add_length = 0) {

  if(exists('refs')) {
    regions <- refs$valueset |>
      dplyr::filter(name == 'area_name_region') |>
      dplyr::collect() |>
      dplyr::transmute(
        region_code = stringr::str_pad(as.integer(value), width = 2, pad = '0'),
        region = label
      )

    regions_long <- refs$valueset |>
      dplyr::filter(name == 'area_name_region_long') |>
      dplyr::collect() |>
      dplyr::transmute(
        region_code = stringr::str_pad(as.integer(value), width = 2, pad = '0'),
        region_long = label
      )
  }

  .data |>
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
}

