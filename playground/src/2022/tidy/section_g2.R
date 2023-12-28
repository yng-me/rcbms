df_temp <- df_temp %>% 
  filter_and_select_regular_hh(prefix = 'g') %>% 
  mutate(
    g20_growing_of_crops_operator_line_number = as.integer(g20_growing_of_crops_operator_line_number),
    g20_crops_cultivated = as.integer(g20_crops_cultivated),
    g20_other_crops_cultivated = as.character(g20_other_crops_cultivated),
    g20_countofcrops = as.integer(g20_countofcrops),
    g20_crop = as.integer(g20_crop)
  ) %>% 
  select_with_geo(everything())