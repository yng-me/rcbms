df_temp <- df_temp |>
  filter_at(vars(matches('^m\\d{2}')), any_vars(!is.na(.))) |> 
  case_id_function('add_count') |> 
  filter(regular_hh_completed == 1, n == 18) |> 
  select(-regular_hh_completed) |> 
  mutate(m19_preparedness_kit_type = rep(c(LETTERS[1:17], 'Z'), nrow(.) / 18)) |> 
  select(-n)