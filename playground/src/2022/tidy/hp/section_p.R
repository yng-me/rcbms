df_temp <- df_temp |> 
  select(-matches('^p0[56]k_comm')) |> 
  filter_and_select_regular_hh(prefix = 'p') |> 
  mutate_at(
    vars(
      matches('^p\\d{2}'),
      -matches('_(other|specified|line_number)$')
    ),
    as.integer
  ) |>
  mutate_at(
    vars(matches('_(other|specified|line_number)$')), 
    as.character
  ) |> 
  select_with_geo(sort(names(.))) |> 
  select(-any_of('regular_hh_completed'))