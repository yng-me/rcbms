df_temp_tidy <- df_temp |> 
  rename(line_number = g09_parcel_operator_line_number) |>
  filter(!is.na(line_number)) |> 
  select_with_geo(line_number, sort(names(.))) |> 
  select(-any_of('regular_hh_completed'))