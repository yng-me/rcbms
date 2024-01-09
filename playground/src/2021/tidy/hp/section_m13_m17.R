df_temp_tidy <- df_temp |> 
  filter_at(vars(matches('^m\\d{2}')), any_vars(!is.na(.))) |> 
  case_id_function('add_count') |> 
  filter(regular_hh_completed == 1, n == 10) |> 
  select(-regular_hh_completed) |> 
  mutate(m13_calamity_type = rep(c(LETTERS[1:9], 'Z'), nrow(.) / 10)) |> 
  select(-n) 