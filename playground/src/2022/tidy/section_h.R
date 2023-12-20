df_temp <- df_temp %>%
  filter_and_select_regular_hh(prefix = 'h') %>%
  mutate_at(
    vars(
      matches('^h\\d{2}'),
      -matches('_(other|specified_\\d)$')
    ),
    as.integer
  ) %>%
  mutate_at(vars(matches('_(other|specified_\\d)$')), as.character) %>%
  mutate(
    # 2021 Pilot CBMS version
    h06_family_income_range = case_when(
      h06_total_family_income < 40000 ~ 1L,
      h06_total_family_income < 60000 ~ 2L,
      h06_total_family_income < 100000 ~ 3L,
      h06_total_family_income < 130000 ~ 4L,
      h06_total_family_income < 260000 ~ 5L,
      h06_total_family_income < 520000 ~ 6L,
      h06_total_family_income < 920000 ~ 7L,
      h06_total_family_income < 1570000 ~ 8L,
      h06_total_family_income < 2620000 ~ 9L,
      h06_total_family_income >= 2620000 ~ 10L
    ),
    # FIES income decile
    h06_family_income_decile = case_when(
      h06_total_family_income < 96000 ~ 1L,
      h06_total_family_income < 119000 ~ 2L,
      h06_total_family_income < 139000 ~ 3L,
      h06_total_family_income < 169000 ~ 4L,
      h06_total_family_income < 199000 ~ 5L,
      h06_total_family_income < 239000 ~ 6L,
      h06_total_family_income < 299000 ~ 7L,
      h06_total_family_income < 379000 ~ 8L,
      h06_total_family_income < 569000 ~ 9L,
      h06_total_family_income >= 569000 ~ 10L
    ),
    # PIDS income cluster
    h06_family_income_cluster = case_when(
      h06_total_family_income < 10957 * 12 ~ 1L,
      h06_total_family_income < 21194 * 12 ~ 2L,
      h06_total_family_income < 43828 * 12 ~ 3L,
      h06_total_family_income < 76669 * 12 ~ 4L,
      h06_total_family_income < 131484 * 12 ~ 5L,
      h06_total_family_income < 219140 * 12 ~ 6L,
      h06_total_family_income >= 219140 * 12 ~ 7L
    )
  ) %>%
  select_with_geo(everything()) %>% 
  select(-any_of('regular_hh_completed'))

