df_temp_tidy <- df_temp |>
  rename(line_number = c09_line_number) |>
  filter(!is.na(line_number))
