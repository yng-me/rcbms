df_temp <- df_temp |> 
  filter_and_select_regular_hh(prefix = 'g') |> 
  mutate_at(
    vars(
      matches('^g\\d{2}'),
      -matches('_(other|specified)$'),
      -matches('^g10_total_physical_area_of_parcels$')
    ), 
    as.integer
  ) |> 
  mutate_at(vars(matches('_(other|specified)$')), as.character) |>
  select_with_geo(everything()) |> 
  select(-any_of('regular_hh_completed'))
