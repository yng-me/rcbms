#' Title
#'
#' @param .data
#' @param .refs
#' @param .config
#'
#' @return
#' @export
#'
#' @examples
#'

select_and_sort_columns <- function(
  .data,
  .refs,
  .config = getOption('rcbms_config')
) {

  df <- dplyr::select(
    .data,
    dplyr::any_of(
      c(
        'case_id',
        'region_code',
        'province_code',
        'city_mun_code',
        'barangay_code',
        'ean',
        'bsn',
        'husn',
        'hsn',
        'line_number'
      )
    ),
    order(names(.data))
  )

  if(tolower(.config$mode$type) == 'portal') {

    included <- .refs$data_dictionary |>
      dplyr::filter(is_included_for_portal == 1) |>
      dplyr::collect() |>
      dplyr::pull(variable_name_new)

    df <- df |>
      dplyr::select(dplyr::any_of(included)) |>
      add_uuid() |>
      normalize_variable()
  }

  return(df)
}
