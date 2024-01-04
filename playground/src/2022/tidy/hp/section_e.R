df_temp_tidy <- df_temp |> 
  pivot_longer_lno() |>
  filter_and_select_regular_hh(prefix = 'e') |> 
  mutate_if(is.double, as.integer) |> 
  mutate(
    line_number = as.integer(line_number),
    e05_work_location = as.integer(e05_work_location),
    e08_psoc = as.integer(e08_psoc),
    e10_psic = str_pad(e10_psic, width = 5, pad = '0'),
    e28_number_of_weeks_looking_for_work = as.integer(e28_number_of_weeks_looking_for_work),
    e29_reason_not_looking_for_work = as.integer(e29_reason_not_looking_for_work),
    e36_last_psoc = as.integer(e36_last_psoc),
    e38_last_psic = as.integer(e38_last_psic)
  ) |> 
  mutate_at(vars(matches('^e1[1-8]')), as.integer) |> 
  mutate_at(vars(matches('^e2[0-6]')), as.integer) |> 
  mutate_at(vars(matches('^e3[0-4]')), as.integer) |> 
  mutate_at(vars(matches('_(other|specified)$')), as.character) |> 
  create_case_id() |>
  distinct(case_id, line_number, .keep_all = T) |> 
  left_join(from_b06_ofi, by = c('case_id', 'line_number')) |>
  left_join(from_section_a, c('case_id', 'line_number')) |> 
  mutate(
    e01_nilf_emp = e01_work_past_week == 2 & e03_job_or_business_past_week %in% c(2, 3),
    e01_nilf_1 = e01_nilf_emp & e25_try_look_for_work_or_do_business == 1 & e31_available_for_work == 2,
    e01_nilf_2 = e01_nilf_emp & e25_try_look_for_work_or_do_business == 2 & e29_reason_not_looking_for_work %in% c(6:10),
    e01_nilf_3 = e01_nilf_emp & e25_try_look_for_work_or_do_business == 2 &
      e29_reason_not_looking_for_work %in% c(0:5) & e31_available_for_work == 2,
    e01_nilf_ofi = b06_ofi %in% c(1:3, 6),
    e01_in_the_labor_force = if_else(e01_nilf_1 | e01_nilf_2 | e01_nilf_3 | e01_nilf_ofi, 2L, 1L, NA_integer_),
    e01_employed = case_when(
      (e01_work_past_week == 1 | e03_job_or_business_past_week == 1) & e01_in_the_labor_force == 1 ~ 1L,
      e01_work_past_week == 2 & e03_job_or_business_past_week %in% c(2, 3) & e01_in_the_labor_force == 1 ~ 2L,
      TRUE ~ NA_integer_
    ),
    e08_occupation_group = if_else(
      e08_psoc == 0, 
      NA_character_, 
      str_pad(as.character(e08_psoc), width = 4, pad = '0')
    ),
    e08_occupation_group = if_else(
      grepl('^0[1-3]', e08_occupation_group), 
      10L, 
      as.integer(str_sub(e08_occupation_group, 1, 1))
    ),
    e10_industry = str_pad(e10_psic, width = 5, pad = '0'),
    e10_industry_group = as.integer(str_sub(e10_industry, 1, 2)),
    e10_industry_group = case_when(
      e10_industry_group %in% 1:3 ~ 1L,
      e10_industry_group %in% 5:9 ~ 2L,
      e10_industry_group %in% 10:33 ~ 3L,
      e10_industry_group %in% 35 ~ 4L,
      e10_industry_group %in% 36:39 ~ 5L,
      e10_industry_group %in% 41:43 ~ 6L,
      e10_industry_group %in% 45:47 ~ 7L,
      e10_industry_group %in% 49:53 ~ 8L,
      e10_industry_group %in% 55:56 ~ 9L,
      e10_industry_group %in% 58:63 ~ 10L,
      e10_industry_group %in% 64:66 ~ 11L,
      e10_industry_group %in% 68 ~ 12L,
      e10_industry_group %in% 69:75 ~ 13L,
      e10_industry_group %in% 77:82 ~ 14L,
      e10_industry_group %in% 84 ~ 15L,
      e10_industry_group %in% 85 ~ 16L,
      e10_industry_group %in% 86:88 ~ 17L,
      e10_industry_group %in% 90:93 ~ 18L,
      e10_industry_group %in% 94:96 ~ 19L,
      e10_industry_group %in% 97:98 ~ 20L,
      e10_industry_group %in% 99 ~ 21L
    )
  ) |>
  select(-c(case_id, e01_nilf_emp, e01_nilf_1, e01_nilf_2, e01_nilf_3, e01_nilf_ofi, e10_industry)) |> 
  select_with_geo(line_number, sex, age, everything()) 



clear_objects(from_b06_ofi, pivot_longer_lno)