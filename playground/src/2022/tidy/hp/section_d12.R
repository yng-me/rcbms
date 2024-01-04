df_temp_tidy <- df_temp |>
  rename(line_number = d12_line_number) |>
  filter(!is.na(line_number))
