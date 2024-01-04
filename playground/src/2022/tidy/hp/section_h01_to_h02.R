df_temp_tidy <- df_temp |>
  rename(line_number = section_h_line_number) |>
  filter(!is.na(line_number))
