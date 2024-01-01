df_temp <- df_temp |> 
  filter_and_select_regular_hh(prefix = 'h') |> 
  mutate_at(vars(matches('^h\\d{2}')), as.integer)
