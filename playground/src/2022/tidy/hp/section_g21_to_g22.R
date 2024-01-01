df_temp <- df_temp |>
  filter(!is.na(g21_crop_equipment_used)) |>
  select(-any_of('regular_hh_completed'))
