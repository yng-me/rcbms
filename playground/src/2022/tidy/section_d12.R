df_temp <- df_temp %>% 
  rename(line_number = d12_line_number) %>%
  filter(!is.na(line_number)) %>% 
  select_with_geo(line_number, sort(names(.))) %>% 
  select(-any_of('regular_hh_completed'))