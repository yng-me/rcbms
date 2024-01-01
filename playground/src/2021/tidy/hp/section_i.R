df_temp <- df_temp |> 
  filter(regular_hh_completed == 1) |>
  select(-regular_hh_completed) |>
  filter_at(vars(matches('^i\\d{2}')), any_vars(!is.na(.))) 