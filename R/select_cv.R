select_cv <- function(.data, ..., .join_hh_info = TRUE, .join_with = NULL) {

  config <- getOption('rcbms_config')
  add_length <- config[[as.character(config$cbms_round)]]$add_length

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
