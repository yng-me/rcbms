#' Title
#'
#' @param .parquet
#' @param .area_name
#' @param .config
#'
#' @return
#' @export
#'
#' @examples
#'

set_aggregation <- function(.parquet, .area_name, .config) {

  if(.config$verbose) {
    cli::cli_h1("Setting Aggregation")
  }

  if (.config$progress) {
    cli::cli_text("Setting aggregation")
  }

  agg <- list()

  agg_level <- .config$aggregation$level
  agg_areas <- .config$aggregation$areas
  agg$levels <- c("all", "region", "province", "city_mun", "barangay", "ean")

  for (i in seq_along(.config$input_data)) {

    input_data <- .config$input_data[[i]]
    agg_record <- .config$project[[input_data]][['summary_record']]

    agg_level_i <- .config$project[[input_data]]$aggregation$level

    if (is.null(agg_level_i)) agg_level_i <- agg_level

    if (agg_level_i > 5) agg_level_i <- 5
    if (agg_level_i < 0) agg_level_i <- 0

    if (!is.null(agg_record)) {

      if(agg_level_i == 5) {
        cols <- (1:5) + 1
      } else {
        cols <- (1:agg_level_i) + 1
      }

      cols_geo <- paste0(agg$levels[cols], "_code")

      df_temp <- .parquet[[input_data]][[agg_record]] |>
        dplyr::select(dplyr::any_of(cols_geo)) |>
        dplyr::distinct() |>
        dplyr::collect() |>
        dplyr::mutate_at(
          dplyr::vars(dplyr::any_of(cols_geo)),
          as.integer
        ) |>
        dplyr::left_join(
          transform_area_name(
            .area_name,
            .add_length = config$project$add_length
          ) |>
          dplyr::mutate_at(
            dplyr::vars(dplyr::any_of(cols_geo)),
            as.integer
          ) |>
          dplyr::select(
            dplyr::any_of(cols_geo),
            dplyr::any_of(agg$levels[cols]),
            dplyr::any_of('is_huc')
          ),
          by = cols_geo
        )

      if('region_code' %in% names(df_temp)) {
        df_temp <- df_temp |>
          dplyr::mutate(region_code = stringr::str_pad(region_code, pad = '0', width = 2))
      }

      if('province_code' %in% names(df_temp)) {
        df_temp <- df_temp |>
          dplyr::mutate(
            province_code = stringr::str_pad(
              province_code, pad = '0', width = 2 + config$project$add_length
            )
          )
      }

      if('city_mun_code' %in% names(df_temp)) {
        df_temp <- df_temp |>
          dplyr::mutate(city_mun_code = stringr::str_pad(city_mun_code, pad = '0', width = 2))
      }

      agg[[input_data]]$areas_unique <- df_temp |>
        tidyr::unite('area_code', dplyr::any_of(cols_geo), sep = '') |>
        tidyr::unite('area_name', dplyr::any_of(rev(agg$levels[cols])), sep = ', ', na.rm = T) |>
        dplyr::distinct()

      agg_areas_i <- config$project[[input_data]]$aggregation$areas
      if (is.null(agg_areas_i)) agg_areas_i <- agg_areas

      if (tolower(agg_areas_i[1]) != "all") {
        if (grepl("\\d+", agg_areas_i)) {
          agg[[input_data]]$areas_unique <- agg[[input_data]]$areas_unique |>
            dplyr::filter(area_code %in% as.integer(agg_areas_i))
        } else {
          agg[[input_data]]$areas_unique <- agg[[input_data]]$areas_unique |>
            dplyr::filter(area_name %in% as.integer(agg_areas_i))
        }
      }

      if (.config$verbose) {
        if (length(.config$input_data) > 1) {
          progress_n <- paste0("[", i, "/", length(.config$input_data), "]: ")
        } else {
          progress_n <- ""
        }
        cli::cli_h3(
          paste0(progress_n, cli::col_br_cyan(get_input_data_label(input_data)))
        )

        areas_cli <- agg[[input_data]]$areas_unique |>
          dplyr::select(area_code, area_name)

        cli::cli_ul(
          paste0(
            cli::style_bold(areas_cli$area_code), " ",
            areas_cli$area_name, " ",
            cli::col_br_cyan("✓")
          )
        )
      }
    }
  }

  return(invisible(set_class(agg, "rcbms_agg")))

}
