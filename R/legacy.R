select_with_geo <- function(.data, ...) {
  .data |>
    dplyr::select(
      dplyr::any_of(
        c(
          'case_id',
          'case_id_gmd',
          'line_number_id',
          'city_mun_geo',
          'barangay_geo',
          'region_code',
          'province_code',
          'city_mun_code',
          'barangay_code',
          'ean',
          'bsn',
          'husn',
          'hsn',
          'region',
          'province',
          'city_mun',
          'barangay',
          'area_name',
          'class',
          'regular_hh_completed',
        )
      ),
      ...,
      dplyr::any_of('is_huc')
    )
}


pivot_longer_lno <- function(.data) {

  df <- list()

  for(lno_i in 1:35) {
    df[[lno_i]] <- .data |>
      dplyr::select(
        1:8,
        dplyr::matches(paste0(stringr::str_pad(lno_i, width = 2, pad = "0"), '$'))
      ) |>
      dplyr::select(-matches('^[b-e]_aux.*')) |>
      dplyr::filter_at(
        dplyr::vars(matches(paste0(stringr::str_pad(lno_i, width = 2, pad = "0"), '$'))),
        dplyr::any_vars(!is.na(.))
      ) |>
      dplyr::rename_at(
        dplyr::vars(dplyr::matches('_\\d{2}$')),
        ~ str_replace(., '_\\d{2}$', '')
      )
  }

  df <- do.call('rbind', df)

  if(mode_type != 'validation') {
    df <- df |>
      dplyr::rename(line_number = lno) |>
      create_barangay_geo() |>
      dplyr::select(-dplyr::contains('aux'))
  }

  return(df)

}


filter_and_select_regular_hh <- function(
  .data,
  .prefix,
  .conform = TRUE,
  .filter_na = TRUE
) {

  p <- paste0('^', .prefix, '\\d{2}')
  aux <- paste0('^', .prefix, '_aux\\d+$')

  if(.conform) {
    .data <- .data |>
      dplyr::mutate(
        regular_hh_completed = dplyr::if_else(
          as.integer(hsn) < 7777,
          1L,
          0L
        )
      )
  }

  if("regular_hh_completed" %in% names(.data)) {
    .data <- .data |>
      dplyr::filter(regular_hh_completed == 1) |>
      dplyr::select(-regular_hh_completed)
  }

  if(.filter_na) {
    .data <- .data |>
      filter_at(
        dplyr::vars(dplyr::matches(p)),
        dplyr::any_vars(!is.na(.))
      )
  }

  .data |>
    dplyr::select(-dplyr::matches(aux)) |>
    dplyr::select(-dplyr::matches('lnoctr$'))

}


select_cv <- function(.data, ..., .join_hh_info = TRUE, .join_with = NULL) {

  config <- getOption('rcbms_config')
  add_length <- config$project$add_length

  .data <- .data |>
    dplyr::mutate(
      ean = str_sub(case_id, 10 + add_length, 15 + add_length),
      barangay_geo = stringr::str_sub(case_id, 1, 9 + add_length)
    )

  if(!('line_number' %in% names(.data))) {
    .data <- .data |>
      dplyr::mutate(line_number = NA_character_)
  }

  area_name <- refs$area_name |>
    transform_area_name() |>
    dplyr::select(barangay_geo)

  .data <- .data |>
    dplyr::select(-dplyr::any_of(c('region', 'province', 'city_mun', 'barangay'))) |>
    dplyr::left_join(area_name, by = 'barangay_geo', multiple = 'first') |>
    dplyr::select(
      case_id,
      region,
      province,
      city_mun,
      barangay,
      ean,
      line_number,
      ...
    )

  if(.join_hh_info & !is.null(.join_with)) {

    .join_with <- .join_with |>
      dplyr::collect() |>
      create_case_id() |>
      dplyr::select(
        dplyr::any_of(
          c(
            'case_id',
            'hh_head',
            'respondent_contact_number',
            'floor_number',
            'subdivision_or_village',
            'sitio_or_purok'
          )
        )
      )

    .data <- .data |>
      dplyr::left_join(.join_with, by = 'case_id')
  }

  return(.data)
}
