df_temp_tidy <- df_temp |>
  filter_and_select_regular_hh(prefix = 's') |>
  mutate_at(vars(matches('^s(0[2-9]|10|11)'), -matches('_other$')), as.integer) |>
  mutate_at(vars(matches('_other$')), as.character) |>
  select_with_geo(everything())