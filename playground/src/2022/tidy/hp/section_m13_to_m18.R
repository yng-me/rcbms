df_temp <- df_temp |> 
  filter_and_select_regular_hh(prefix = 'm') |> 
  mutate_at(vars(matches('_specified$')), as.character) |> 
  select_with_geo(sort(names(.))) |> 
  select(-any_of('regular_hh_completed'))