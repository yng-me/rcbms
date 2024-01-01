df_temp <- df_temp |> 
  filter_and_select_regular_hh(prefix = 'i') |> 
  mutate_at(vars(matches('^i\\d{2}')), as.integer) |> 
  select_with_geo(everything()) |> 
  select(-any_of('regular_hh_completed'))