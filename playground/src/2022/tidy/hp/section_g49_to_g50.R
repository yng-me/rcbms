df_temp_tidy <- df_temp |>
  filter_and_select_regular_hh(prefix = 'g') |>
  select(-any_of('regular_hh_completed'))
