#' Title
#'
#' @param .parquet
#' @param .references
#' @param ...
#' @param .config
#'
#' @return
#' @export
#'
#' @examples
#'

set_aggregation <- function(
  ...,
  .parquet = get_config("parquet"),
  .references = get_config("references"),
  .config = getOption("rcbms.config"),
  .input_data = "hp",
  .update_config = TRUE,
  .config_key = "aggregation"
) {

  agg_record <- get_summary_record(.input_data)

  if(is.null(agg_record)) stop('Summary record is not defined.')

  agg <- list()
  agg_level <- .config$aggregation$level
  if(.config$aggregation$level > 4) agg_level <- 4
  if(.config$aggregation$level < 1) agg_level <- 1

  agg$levels <- c('barangay', 'city_mun', 'province', 'region', 'all_area')
  agg$labels <- c('Barangay', 'City/Municipality', 'Province', 'Region', 'Philippines')

  agg$value <- agg$levels[agg_level]
  agg$label <- agg$labels[agg_level]

  agg$areas_all <- .parquet[[.input_data]][[agg_record]] |>
    dplyr::collect() |>
    create_barangay_geo() |>
    dplyr::select(-dplyr::contains('_code')) |>
    dplyr::left_join(
      transform_area_name(.references, .config$project$add_length),
      by = 'barangay_geo'
    ) |>
    dplyr::select(
      dplyr::starts_with(c('region', 'province', 'city_mun', 'barangay')),
      dplyr::any_of("is_huc")
    ) |>
    dplyr::distinct() |>
    tidyr::drop_na() |>
    dplyr::mutate(
      all_area_geo = "",
      all_area_agg = "All Areas",
      region_geo = region_code,
      .before = 1
    )

  geo_name <- paste0(agg$levels[agg_level + 1], "_geo")
  geo_agg <- paste0(agg$levels[agg_level + 1], "_agg")

  agg$areas_unique <- agg$areas_all |>
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

  envir <- as.environment(1)

  if(!is.null(.config_key) & .update_config) {
    .config$links$aggregation <- .config_key
    options(rcbms.config = .config)

    assign("config", .config, envir = envir)
  }

  agg <- set_class(agg, "rcbms_agg")
  assign(.config_key, agg, envir = envir)

  return(invisible(agg))

}





