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
  if(.config$aggregation$level > 5) agg_level <- 5
  if(.config$aggregation$level < 1) agg_level <- 1

  agg$levels <- c('barangay', 'city_mun', 'province', 'region', 'all_area')
  agg$labels <- c('Barangay', 'City/Municipality', 'Province', 'Region', 'Philippines')

  agg$value <- agg$levels[agg_level]
  agg$label <- agg$labels[agg_level]

  agg$areas_all <- .parquet[[input_data]][[agg_record]] |>
    dplyr::collect() |>
    create_barangay_geo() |>
    dplyr::select(-contains('_code')) |>
    dplyr::left_join(
      transform_area_name(.refs, .config$project$add_length),
      by = 'barangay_geo'
    ) |>
    dplyr::select(
      dplyr::starts_with(c('region', 'province', 'city_mun', 'barangay')),
      is_huc
    ) |>
    dplyr::distinct() |>
    tidyr::drop_na()

  geo_name <- paste0(agg$value, '_geo')
  geo_agg <- paste0(agg$value, '_agg')
  agg$areas_unique <- agg$areas_all |>
    dplyr::mutate(
      region_geo = region_code,
      all_area_geo = '',
      all_area_agg = 'All Areas'
    ) |>
    dplyr::select(dplyr::any_of(c(geo_name, geo_agg))) |>
    dplyr::distinct(!!as.name(geo_name), .keep_all = T) |>
    dplyr::rename(
      code = !!as.name(geo_name),
      label = !!as.name(geo_agg)
    )

  if(tolower(config$aggregation$areas[1]) != 'all') {

    if(grepl('\\d+', .config$aggregation$areas)) {

      agg$areas_unique <- agg$areas_unique |>
        dplyr::filter(code %in% .config$aggregation$areas)

    } else {

      agg$areas_unique <- agg$areas_unique |>
        dplyr::filter(label %in% .config$aggregation$areas)
    }
  }

  # .config$aggregation <- c(.config$aggregation, agg)
  # options(rcbms_config = .config)

  return(agg)

}
