df_temp <- df_temp %>% 
  filter_and_select_regular_hh(prefix = 'l') %>% 
  mutate_at(vars(
    matches('^l\\d{2}'), 
    -matches('(_other|specified)$'), 
    -matches('^(l39_medical_treatment_provider|l40_payment_for_treatment)')), 
    as.integer
  ) %>% 
  mutate_at(vars(matches('_(other|specified)$')), as.character) %>% 
  select_with_geo(everything())
