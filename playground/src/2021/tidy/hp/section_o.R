df_temp_tidy <- df_temp |> 
  filter(regular_hh_completed == 1) |> 
  select(-regular_hh_completed) |> 
  filter_at(vars(matches('^o\\d{2}')), any_vars(!is.na(.))) |> 
  mutate(o01_safe_walking_alone_fct = add_factor(o01_safe_walking_alone, code_ref = 'o01'))