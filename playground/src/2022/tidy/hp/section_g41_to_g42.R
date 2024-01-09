df_temp_tidy <- df_temp |>
  filter(!is.na(g41_aquafarm_equipment_used)) |>
  select(-any_of('regular_hh_completed'))
