df_temp <- df_temp |>
  filter(!is.na(g28_livestock_poultry_equipment_used)) |>
  select(-any_of('regular_hh_completed'))
