join_and_filter_area <- function(
  .data,
  .retain_agg = F,
  .filter_completed = T,
  .minimized = T
) {

  area <- refs$area_name |>
    dplyr::collect() |>
    dplyr::select(-dplyr::ends_with('code'))

  if(minimized == T) {
    area <- area |>
      dplyr::select(
        -dplyr::any_of(
          c(
          'x2015_popn',
          'x2020_popn',
          'funding_source',
          'psu_weight',
          'sample_psu'
        )
      )
    )
  }

  df <- .data |>
    left_join(area, by = 'barangay_geo') |>
    mutate(aggregate_level = !!as.name(paste0(aggregation$levels[n_level], '_agg'))) |>
    filter(!is.na(aggregate_level))

  if(retain_agg == F) {
    df <- df |>
      select(-ends_with('_agg'))
  }

  if(!exists('selected_input')) {
    selected_input <- input_data[1]
  }

  if(!('case_id' %in% names(.data)) & selected_input == 'hp') {
    df <- df |>
      create_case_id(filter_completed)
  }

  if(exists('completed_cases') & filter_completed) {
    df <- df |> filter(case_id %in% completed_cases)
  }

  if(exists('incomplete_cases') & filter_completed) {
    df <- df |> filter(!(case_id %in% incomplete_cases))
  }

  if(n_level < length(aggregation$levels)) {
    df <- df |>
      filter(!!as.name(paste0(aggregation$levels[n_level + 1], '_geo')) == agg_area_code)
  }

  if(exclude_specify_field == T) {
    df <- df |>
      select(-ends_with('other_specified'), -ends_with('_specified'))
  }
  return(df)
}
