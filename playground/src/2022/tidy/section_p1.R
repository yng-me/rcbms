df_temp <- df_temp %>% 
  filter_and_select_regular_hh(prefix = 'p') %>% 
  mutate_at(vars(matches('^p\\d{2}')), as.integer)