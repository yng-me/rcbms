df_temp <- df_temp %>% 
  filter(regular_hh_completed == 1) %>%
  select(-regular_hh_completed) %>%
  filter_at(vars(matches('^h\\d{2}')), any_vars(!is.na(.))) %>% 
  mutate(
    h04_major_industry_code = if_else(
      h04_major_industry_code == '0' | str_trim(h04_major_industry_details) == 'CROP FARMING',
      'A',
      h04_major_industry_code
    ),
    h04_major_industry_code = if_else(
      is.na(h04_major_industry_code), 
      str_sub(h04_major_industry_name, 1, 1),
      h04_major_industry_code
    ),
    h04_major_industry_code = if_else(
      (is.na(h04_major_industry_code) | h04_major_industry_code == '0') & h05_entrep_psic %in% c(4510:4799), 
      'J',
      h04_major_industry_code
    )
  ) %>% 
  mutate(
    h04_major_industry_group = case_when(
      h04_major_industry_code %in% LETTERS[1:5] ~ 1L,
      h04_major_industry_code %in% LETTERS[5:9] ~ 2L,
      h04_major_industry_code %in% LETTERS[10:22] ~ 3L
    ),
    h06_online_presence = case_when(
      h06_entrep_use_ecommerce == 1 & h07_entrep_use_social_media == 1 ~ 1L,
      h06_entrep_use_ecommerce == 1 & h07_entrep_use_social_media != 1 ~ 2L,
      h06_entrep_use_ecommerce != 1 & h07_entrep_use_social_media == 1 ~ 3L,
      h06_entrep_use_ecommerce != 1 & h07_entrep_use_social_media != 1 ~ 4L,
      TRUE ~ NA_integer_
    ),
    h08_entrep_year_started_range = case_when(
      h08_entrep_year_started >= 2020 ~ 1L,
      h08_entrep_year_started >= 2015 ~ 2L,
      h08_entrep_year_started >= 2010 ~ 3L,
      h08_entrep_year_started >= 2000 ~ 4L,
      h08_entrep_year_started >= 1990 ~ 5L,
      h08_entrep_year_started >= 1980 ~ 6L,
      h08_entrep_year_started >= 1970 ~ 7L,
      h08_entrep_year_started >= 1950 ~ 8L,
      h08_entrep_year_started < 1950 ~ 9L
    ),
    h10_a_number_of_working_owners = if_else(is.na(h10_a_number_of_working_owners), 0L, h10_a_number_of_working_owners),
    h10_b_number_of_paid_employees = if_else(is.na(h10_b_number_of_paid_employees), 0L, h10_b_number_of_paid_employees),
    h10_c_total_employees = h10_a_number_of_working_owners + h10_b_number_of_paid_employees
  ) %>% 
  mutate(
    h04_major_industry_group_fct = add_factor(h04_major_industry_group, code_ref = 'h03_major'),
    h04_major_industry_fct = add_factor(h04_major_industry_code, code_ref = 'h03_short_letter', is_char = T),
    h06_online_presence_fct = add_factor(h06_online_presence, code_ref = 'h06'),
    h08_entrep_year_started_range_fct = add_factor(h08_entrep_year_started_range, code_ref = 'h08')
  )