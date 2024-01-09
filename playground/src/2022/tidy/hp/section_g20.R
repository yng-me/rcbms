df_temp_tidy <- df_temp |>
  rename(line_number = g20_growing_of_crops_operator_line_number) |>
  filter(!is.na(line_number)) |>
  select(-any_of('regular_hh_completed'))
