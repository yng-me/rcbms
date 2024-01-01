df_temp <- df_temp |>
  filter(!is.na(g52_fishing_equipment_used)) |>
  select(-any_of('regular_hh_completed'))
