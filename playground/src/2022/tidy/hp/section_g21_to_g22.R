df_temp <- df_temp %>%
  filter(!is.na(g21_crop_equipment_used)) %>%
  select_with_geo(sort(names(.))) %>% 
  select(-any_of('regular_hh_completed'))