df_temp_tidy <- df_temp |> 
  filter_and_select_regular_hh(prefix = 'l') |> 
  select_with_geo(sort(names(.))) |> 
  select(-any_of('regular_hh_completed'))
