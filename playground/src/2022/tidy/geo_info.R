df_temp <- df_temp %>% 
  mutate_at(
    vars(any_of(c(
    'floor_number',
    'house_number',
    'block_or_lot_number',
    'street_name',
    'subdivision_village',
    'sitio_purok')
  )), as.character) %>% 
  select_with_geo(longitude, latitude, everything()) %>% 
  select(-any_of('regular_hh_completed'))
