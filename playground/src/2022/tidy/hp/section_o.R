df_temp <- df_temp |> 
  filter_and_select_regular_hh(prefix = 'o') |> 
  mutate_at(vars(matches('^o\\d{2}')), as.integer) |>
  select_with_geo(sort(names(.))) |> 
  select(-any_of('regular_hh_completed'))