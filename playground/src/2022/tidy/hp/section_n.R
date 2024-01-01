df_temp <- df_temp |> 
  filter_and_select_regular_hh(prefix = 'n') |> 
  mutate_at(
    vars(matches('^n\\d{2}'), -matches('_specified_\\d$')), 
    as.integer
  ) |> 
  mutate_at(vars(matches('specified_\\d$')), as.character) |> 
  select_with_geo(sort(names(.))) |> 
  select(-any_of('regular_hh_completed'))