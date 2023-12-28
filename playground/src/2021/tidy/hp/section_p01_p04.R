df_temp <- df_temp %>%
  filter_at(vars(matches('^p\\d{2}')), any_vars(!is.na(.))) %>% 
  case_id_function('add_count') %>% 
  filter(regular_hh_completed == 1, n == 7) %>% 
  select(-regular_hh_completed) %>% 
  mutate(p01_insurance_program_type = rep(c(LETTERS[1:7]), nrow(.) / 7)) %>% 
  select(-n)