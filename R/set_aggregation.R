set_aggregation <- function(.config = getOption('rcbms_config')) {

  agg_record <- get_summary_record(.config$input_data[1])

  if(is.null(agg_record)) stop('Summary record is not defined.')

  agg <- list()
  agg_level <- .config$aggregation_level
  agg_levels <- c('barangay', 'city_mun', 'province', 'region', 'country')

  agg$level <- agg_levels[agg_level]

  agg$areas_all <- parquet[[input_data[1]]][[agg_record]] |>
        select(-contains('_code')) |>
        left_join(refs$area_name, by = 'barangay_geo') |>
        select(
          starts_with(c('region', 'province', 'city_mun', 'barangay')),
          is_huc
        ) |>
        distinct() |>
        collect() |>
        drop_na() |>
        mutate(
          n_level = n_level,
          n_levels = length(aggregation$levels)
        ) |>
        mutate(
          area_name = ifelse(
            n_levels == n_level,
            aggregation$top_level_name[1],
            !!as.name(paste0(aggregation$levels[n_level + 1], '_agg'))
          ),
          code = ifelse(
            n_levels == n_level,
            '999', # Arbitrarily set to a uniform value for aggregation
            !!as.name(paste0(aggregation$levels[n_level + 1], '_geo'))
          )
        ) |>
        select(
          area = !!as.name(paste0(aggregation$levels[n_level], '_agg')),
          geo = !!as.name(paste0(aggregation$levels[n_level], '_geo')),
          by = area_name,
          code
        ) |>
        distinct(geo, .keep_all = T)

      if(aggregation$areas[1] == 'all') {
        aggregation$areas_unique <- unique(aggregation$areas_all$by)
        aggregation$areas_unique_code <- unique(aggregation$areas_all$code)
      } else {
        aggregation$areas_unique <- aggregation$areas
        aggregation$areas_unique_code <- aggregation$areas_all |>
          filter(by %in% aggregation$areas | code %in% aggregation$areas) |>
          distinct(code) |>
          pull()
      }

    }
}
