df_temp <- df_temp |> 
  filter_and_select_regular_hh(prefix = 'o') |> 
  mutate_at(vars(matches('^o\\d{2}'), -matches('(_other|specified)$')), as.integer) |> 
  mutate_at(vars(matches('_(other|specified)$')), as.character) |> 
  select_with_geo(everything()) 