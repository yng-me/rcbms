df_temp <- df_temp %>%
  filter_at(vars(matches('^i\\d{2}')), any_vars(!is.na(.))) %>% 
  case_id_function('add_count') %>% 
  filter(regular_hh_completed == 1, n == 20) %>% 
  select(-regular_hh_completed) %>% 
  mutate(i04_use_of_financial_account_type = rep(LETTERS[1:20], nrow(.) / 20)) %>% 
  select(-n) %>% 
  mutate(
    i04_use_of_financial_account_group = case_when(
      i04_use_of_financial_account_type %in% LETTERS[1:7] ~ 1L,
      i04_use_of_financial_account_type %in% LETTERS[8:14] ~ 2L,
      i04_use_of_financial_account_type %in% LETTERS[15:19] ~ 3L,
      i04_use_of_financial_account_type %in% LETTERS[20] ~ 4L
    )
  ) %>% 
  mutate(
    i04_use_of_financial_account_group_fct = add_factor(i04_use_of_financial_account_group, code_ref = 'i04')
  )
  
