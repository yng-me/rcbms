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
  .update_config = TRUE,
  .config_key = "aggregation"
) {

  if(.config$verbose) {
    cli::cli_h1("Setting Aggregation")
  }

  agg <- list()

  agg_level <- .config$aggregation$level
  agg_areas <- .config$aggregation$areas
  agg$levels <- c('barangay', 'city_mun', 'province', 'region', 'all_area')
  agg$labels <- c('Barangay', 'City/Municipality', 'Province', 'Region', 'Philippines')


  for(i in seq_along(.config$input_data)) {

    input_data <- .config$input_data[[i]]
    agg_record <- get_summary_record(input_data)

    agg_level_i <- .config$project[[input_data]]$aggregation$level

    if(is.null(agg_level_i)) agg_level_i <- agg_level
    if(agg_level_i > 4) agg_level_i <- 4
    if(agg_level_i < 1) agg_level_i <- 1

    agg[[input_data]]$value <- agg$levels[agg_level_i]
    agg[[input_data]]$label <- agg$labels[agg_level_i]

    if(!is.null(agg_record)) {
      agg[[input_data]]$areas_all <- .parquet[[input_data]][[agg_record]] |>
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

      geo_name <- paste0(agg$levels[agg_level_i + 1], "_geo")
      geo_agg <- paste0(agg$levels[agg_level_i + 1], "_agg")

      agg[[input_data]]$areas_unique <- agg[[input_data]]$areas_all |>
        dplyr::select(dplyr::any_of(c(geo_name, geo_agg))) |>
        dplyr::distinct(!!as.name(geo_name), .keep_all = T) |>
        dplyr::rename(
          code = !!as.name(geo_name),
          label = !!as.name(geo_agg)
        )

      agg_areas_i <- .config$project[[input_data]]$aggregation$areas
      if(is.null(agg_areas_i)) agg_areas_i <- agg_areas

      if(tolower(agg_areas_i[1]) != 'all') {

        if(grepl('\\d+', agg_areas_i)) {

          agg[[input_data]]$areas_unique <- agg[[input_data]]$areas_unique |>
            dplyr::filter(code %in% agg_areas_i)

        } else {

          agg[[input_data]]$areas_unique <- agg[[input_data]]$areas_unique |>
            dplyr::filter(label %in% agg_areas_i)
        }
      }

      if(.config$verbose) {

        if(length(.config$input_data) > 1) {
          progress_n <- paste0("[", i, "/", length(.config$input_data), "]: ")
        } else {
          progress_n <- ""
        }
        cli::cli_h3(
          paste0(progress_n, cli::col_br_cyan(get_input_data_label(input_data)))
        )

        areas_cli <- agg[[input_data]]$areas_unique |>
          dplyr::select(code, label)

        for(k in seq_along(areas_cli$label)) {
          cli::cli_alert_info(
            paste0(
              cli::style_bold(areas_cli$code[k]), " ",
              areas_cli$label[k], " ",
              cli::col_br_cyan("✓")
            )
          )
        }
      }

    }
  }

  envir <- as.environment(1)

  if(!is.null(.config_key) && .update_config) {
    .config$links$aggregation <- .config_key
    options(rcbms.config = .config)

    assign("config", .config, envir = envir)
  }

  agg <- set_class(agg, "rcbms_agg")
  assign(.config_key, agg, envir = envir)

  return(invisible(agg))

}





