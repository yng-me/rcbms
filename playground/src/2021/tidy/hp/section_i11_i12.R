df_temp_tidy <- df_temp |>
  filter_at(vars(matches('^i\\d{2}')), any_vars(!is.na(.))) |> 
  case_id_function('add_count') |> 
  filter(regular_hh_completed == 1, n == 12) |> 
  select(-regular_hh_completed) |> 
  mutate(i11_insurance_products = if_else(!(i11_insurance_products %in% c(1, 2)), 2L, i11_insurance_products)) |> 
  mutate(i11_insurance_products_type = rep(LETTERS[1:12], nrow(.) / 12)) |> 
  select(-n) 