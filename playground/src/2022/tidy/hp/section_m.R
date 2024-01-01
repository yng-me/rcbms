df_temp <- df_temp |> 
  filter_and_select_regular_hh(prefix = 'm', filter_na = F) |> 
  mutate_at(
    vars(
      matches('^m\\d{2}'), 
      -matches('m11_evacuated_location_other|_specified$')
    ), 
    as.integer
  ) |> 
  mutate_at(vars(matches('_specified$')), as.character) |> 
  select_with_geo(sort(names(.))) |> 
  select(-any_of('regular_hh_completed'))
