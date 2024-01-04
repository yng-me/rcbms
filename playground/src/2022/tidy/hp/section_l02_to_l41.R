df_temp_tidy <- df_temp |> 
  rename(line_number = section_l_line_number) |>
  mutate_at(vars(matches('_(other|specified)$')), as.character) |> 
  filter(!is.na(line_number)) |> 
  select_with_geo(line_number, sort(names(.))) |> 
  select(-any_of('regular_hh_completed'))