df_temp_tidy <- df_temp |>
  filter_at(vars(matches('^h\\d{2}')), any_vars(!is.na(.))) |> 
  case_id_function('add_count') |> 
  filter(regular_hh_completed == 1, n == 22) |> 
  select(-regular_hh_completed) |> 
  mutate(h03_entrep_activity_type = rep(1:22, nrow(.) / 22)) |> 
  select(-n) |> 
  mutate(
    h03_major_industry_group = case_when(
      h03_entrep_activity_type %in% c(1:4) ~ 1L,
      h03_entrep_activity_type %in% c(5:9) ~ 2L,
      h03_entrep_activity_type %in% c(10:22) ~ 3L
    )
  ) |> 
  mutate(
    h03_entrep_activity_type_fct = add_factor(h03_entrep_activity_type, code_ref = 'h03_short'),
    h03_major_industry_group_fct = add_factor(h03_major_industry_group, code_ref = 'h03_major')
  )