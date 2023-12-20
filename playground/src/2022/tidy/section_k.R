df_temp <- df_temp %>% 
  filter_and_select_regular_hh(prefix = 'k') %>% 
  mutate_at(vars(matches('^k\\d{2}'), -matches('_specified$')), as.integer) %>% 
  mutate_at(vars(matches('_specified$')), as.character) %>% 
  select_with_geo(everything()) %>% 
  select(-any_of('regular_hh_completed'))