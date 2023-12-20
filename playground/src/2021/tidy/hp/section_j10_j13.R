df_temp <- df_temp %>% 
  filter(regular_hh_completed == 1) %>% 
  select(-regular_hh_completed) %>% 
  filter_at(vars(matches('^j\\d{2}')), any_vars(!is.na(.))) %>% 
  mutate(j13_cause_of_death_fct = add_factor(j13_cause_of_death, code_ref = 'j13'))