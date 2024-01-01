df_temp <- df_temp |>
  rename(line_number = d08_line_number) |>
  filter(!is.na(line_number))
