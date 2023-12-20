#' Title
#'
#' @param .parquet
#' @param .refs
#' @param ...
#' @param .config
#'
#' @return
#' @export
#'
#' @examples
#'

set_aggregation <- function(
  .parquet,
  .refs,
  ...,
  .config = getOption('rcbms_config')
) {

  input_data <- .config$input_data[1]
  agg_record <- get_summary_record(input_data)

  if(is.null(agg_record)) stop('Summary record is not defined.')

  agg <- list()
  agg_level <- .config$aggregation$level
  agg_levels <- c('barangay', 'city_mun', 'province', 'region', 'country')
  agg_labels <- c('Barangay', 'City/Municipality', 'Province', 'Region', 'Philippines')

  agg$level <- agg_levels[agg_level]
  if(agg_levels[agg_level] > 4) agg$level <- 4

  agg$areas <- .parquet[[input_data]][[agg_record]] |>
    dplyr::collect() |>
    create_barangay_geo() |>
    dplyr::select(-contains('_code')) |>
    dplyr::left_join(
      transform_area_name(
        dplyr::collect(.refs$area_name),
        .refs = refs,
        .add_length = .config$project$add_length
      ),
      by = 'barangay_geo'
    ) |>
    dplyr::select(
      dplyr::starts_with(c('region', 'province', 'city_mun', 'barangay')),
      is_huc
    ) |>
    dplyr::distinct() |>
    tidyr::drop_na()

  agg$areas_unique <- agg$areas |>
    dplyr::select(dplyr::any_of(paste0(agg$level, '_geo'))) |>
    dplyr::distinct() |>
    dplyr::pull()

  return(agg)

}
