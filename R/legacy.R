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
          'regular_hh_completed'
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
      dplyr::select(-dplyr::matches('^[b-e]_aux.*')) |>
      dplyr::filter_at(
        dplyr::vars(dplyr::matches(paste0(stringr::str_pad(lno_i, width = 2, pad = "0"), '$'))),
        dplyr::any_vars(!is.na(.))
      ) |>
      dplyr::rename_at(
        dplyr::vars(dplyr::matches('_\\d{2}$')),
        ~ stringr::str_replace(., '_\\d{2}$', '')
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
      dplyr::filter_at(
        dplyr::vars(dplyr::matches(p)),
        dplyr::any_vars(!is.na(.))
      )
  }

  .data |>
    dplyr::select(-dplyr::matches(aux)) |>
    dplyr::select(-dplyr::matches('lnoctr$'))

}



#' Title
#'
#' @param .data
#'
#' @return
#' @export
#'
#' @examples
#'

mutate_line_number <- function(.data) {

  ln <- .data |>
    dplyr::group_by(case_id) |>
    dplyr::count()

  max <- max(ln$n, na.rm = T)

  if(max > 0) {

    for(i in seq_along(max)) {
      .data <- .data |>
        dplyr::mutate(line_number = dplyr::if_else(
          case_id == dplyr::lag(case_id) & is.na(line_number),
          as.integer(dplyr::lag(line_number)) + 1L,
          line_number
        )
        )
    }
  }

  return(.data)
}
