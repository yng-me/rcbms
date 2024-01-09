df_temp_tidy <- df_temp |>
  filter_at(vars(matches('^p\\d{2}')), any_vars(!is.na(.))) |> 
  case_id_function('add_count') |> 
  filter(regular_hh_completed == 1, n == 10) |> 
  select(-regular_hh_completed) |> 
  mutate(p05_social_assistance_type = rep(c(LETTERS[1:10]), nrow(.) / 10)) |> 
  select(-n)