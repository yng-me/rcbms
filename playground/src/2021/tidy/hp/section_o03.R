df_temp <- df_temp %>%
  filter_at(vars(matches('^o\\d{2}')), any_vars(!is.na(.))) %>% 
  case_id_function('add_count') %>% 
  filter(regular_hh_completed == 1, n == 12) %>% 
  select(-regular_hh_completed) %>% 
  mutate(o02_type_of_crime_type = rep(c(LETTERS[1:11], 'Z'), nrow(.) / 12)) %>% 
  select(-n)