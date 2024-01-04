df_temp_tidy <- df_temp |>
  filter_at(vars(matches('^p\\d{2}')), any_vars(!is.na(.))) |> 
  case_id_function('add_count') |> 
  filter(regular_hh_completed == 1, n == 6) |> 
  select(-regular_hh_completed) |> 
  mutate(p11_labor_market_intervention_type = rep(c(LETTERS[1:6]), nrow(.) / 6)) |> 
  select(-n)