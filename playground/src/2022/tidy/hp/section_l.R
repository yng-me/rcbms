df_temp <- df_temp |> 
  filter_and_select_regular_hh(prefix = 'l') |> 
  mutate_at(vars(matches('^l\\d{2}'), -matches('(l11_name_of_the_child|_other|specified)$')), as.integer) |> 
  mutate_at(vars(matches('_(other|specified)$')), as.character) |> 
  select_with_geo(sort(names(.))) |> 
  select(-any_of('regular_hh_completed'))
