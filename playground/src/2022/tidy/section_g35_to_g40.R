df_temp <- df_temp %>%
  rename(line_number = g35_aquafarm_operator_line_number) %>%
  filter(!is.na(line_number)) %>% 
  select_with_geo(line_number, sort(names(.))) %>% 
  select(-any_of('regular_hh_completed'))