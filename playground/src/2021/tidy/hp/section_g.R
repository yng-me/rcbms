df_temp <- df_temp %>% 
  filter(regular_hh_completed == 1) %>% 
  select(-regular_hh_completed) %>% 
  filter_at(vars(matches('^g\\d{2}')), any_vars(!is.na(.))) %>% 
  mutate_at(
    vars(matches('^g01_[a-n]_.*')), 
    ~ if_else(is.na(.), 0L, as.integer(.))
  ) %>% 
  mutate(
    g02_total_weekly_food_consumption = if_else(
      g02_total_weekly_food_consumption != rowSums(select(., matches('^g01_[a-n]_.*')), na.rm = T),
      rowSums(select(., matches('^g01_[a-n]_.*')), na.rm = T),
      g02_total_weekly_food_consumption
    )
  )
