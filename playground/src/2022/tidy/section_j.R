df_temp <- df_temp %>% 
  filter_and_select_regular_hh(prefix = 'j') %>% 
  mutate_at(vars(matches('^j\\d{2}')), as.integer) %>% 
  filter_at(vars(matches('^j0[1-9]_')), any_vars(!is.na(.))) %>%
  mutate_at(vars(matches('^j0[1-8]_')), list(new = ~ if_else(. %in% c(1, 2), ., NA_integer_, NA_integer_))) %>%
  mutate_at(vars(matches('^j0[1-8]_')), ~ if_else(. == 2, 0L, .)) %>%
  mutate(j10_raw_score = rowSums(select(., matches('^j0[1-9]_.*_new$')), na.rm = T)) %>% 
  mutate(j10_raw_score = as.integer(j10_raw_score)) %>% 
  rename_at(vars(matches('^j0[1-8]_.*_new$')), ~ paste0('j11_', letters[as.integer(str_sub(., 3, 3))])) %>% 
  select_with_geo(everything()) %>% 
  select(-any_of('regular_hh_completed'))