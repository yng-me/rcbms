df_temp <- df_temp |>
  filter_at(vars(matches('^p\\d{2}')), any_vars(!is.na(.))) |> 
  case_id_function('add_count') |> 
  filter(regular_hh_completed == 1, n == 13) |> 
  select(-regular_hh_completed) |> 
  mutate(p17_bayanihan_act_type = rep(c(LETTERS[1:13]), nrow(.) / 13)) |> 
  select(-n)