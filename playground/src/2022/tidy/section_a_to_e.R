tidy_cbms_data_temp <- function(.data) {

  .data |>
    mutate(
      line_number = as.integer(line_number),
      a02_relation_to_hh_head = as.integer(a02_relation_to_hh_head),
      a03_nuclear_family = as.integer(a03_nuclear_family),
      a04_relation_to_nuclear_family_head = as.integer(a04_relation_to_nuclear_family_head),
      a07_age = as.integer(a07_age),
      a10_ethnicity_other = as.character(a10_ethnicity_other),
      a11_religion_other = as.character(a11_religion_other),
      a16_lgu_id_number = as.character(a16_lgu_id_number),
      a09_marital_status = if_else(is.na(a09_marital_status) & age < 15, 1L, a09_marital_status),
      a12_phil_id_ownership = case_when(
        a12_phil_id == 1 ~ 1L,
        a12_phil_id %in% c(2, 8) & a14_phil_id_step_2 == 1 ~ 2L,
        a12_phil_id == 2 ~ 3L,
        a12_phil_id == 8 | a14_phil_id_step_2 == 8 ~ 8L
      )
    ) |>
    filter(!is.na(a05_sex), !is.na(a07_age)) |>
    convert_to_na() |>
    add_age_groups(a07_age, .prefix = 'a07') |>
    add_ip_group(a10_ethnicity, .prefix = 'a10') |>
    add_wgss(.prefix = 'a20') |>
    mutate(
      b01_migrant_life_time = if_else(b01_mother_resided > 1, 1L, 2L, NA_integer_),
      b02_migrant_5_years = if_else(b02_resided_5_years_ago > 1, 1L, 2L, NA_integer_),
      b04_migrant_6_months = if_else(b04_resided_6_months_ago > 1, 1L, 2L, NA_integer_)
    ) |>
    create_case_id() |>
    mutate(
      c02_hgc_group = case_when(
        c02_hgc <= 2000 ~ 1L, # no grade completed / early childhood education,
        c02_hgc <= 10017 ~ 2L, # elementary level,
        c02_hgc == 10018 ~ 3L, # elementary graduate,
        c02_hgc <= 24014 ~ 4L, # junior high school level,
        c02_hgc <= 24024 ~ 5L, # junior high school graduate,
        c02_hgc %in% c(34011, 34012, 34021, 34022, 34031, 34032, 35011, 35012) ~ 6L,
        c02_hgc %in% c(34013:34018, 34023:34028, 34033, 35013:35018) ~ 7L, # senior high school graduate,
        c02_hgc %in% c(40001:40003) ~ 8L, # post-secondary non-tertiary level,
        c02_hgc %in% c(40011:49999) ~ 9L, # post-secondary non-tertiary graduate,
        c02_hgc %in% c(50001:50003) ~ 10L, # short-cycle tertiary level,
        c02_hgc %in% c(50011:59999) ~ 11L, # short-cycle tertiary graduate,
        c02_hgc %in% c(60001:60006) ~ 12L, # college level,
        c02_hgc %in% c(69998:69999) ~ 13L, # college graduate,
        c02_hgc == 70010 ~ 14L, # masteral level,
        c02_hgc %in% c(79998:79999) ~ 15L, # masteral graduate,
        c02_hgc == 80010 ~ 16L, # doctoral level,
        c02_hgc > 80010 ~17L # college graduate or higher
      )
    ) |>
    mutate_at(vars(matches('^d(08|12)_.*_work_[2-6]$')), as.integer) |>
    mutate_at(vars(matches('^d(08|12)_.*_specified$')), as.character) |>
    mutate(
      d01_voter = case_when(
        d01_registered_voter == 1 & d02_voted_last_elections == 1 ~ 1L,
        d01_registered_voter == 1 & d02_voted_last_elections != 1 ~ 2L,
        d01_registered_voter == 2 ~ 3L
      )
    ) |>
    mutate(
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
    mutate(
      e01_nilf_emp = e01_work_past_week == 2 & e03_job_or_business_past_week %in% c(2, 3),
      e01_nilf_1 = e01_nilf_emp & e25_try_look_for_work_or_do_business == 1 & e31_available_for_work == 2,
      e01_nilf_2 = e01_nilf_emp & e25_try_look_for_work_or_do_business == 2 &
        e29_reason_not_looking_for_work %in% c(6:10),
      e01_nilf_3 = e01_nilf_emp & e25_try_look_for_work_or_do_business == 2 &
        e29_reason_not_looking_for_work %in% c(0:5) & e31_available_for_work == 2,
      e01_nilf_ofi = b06_ofi %in% c(1:3, 6),
      e01_in_the_labor_force = if_else(
        e01_nilf_1 | e01_nilf_2 | e01_nilf_3 | e01_nilf_ofi,
        2L,
        1L,
        NA_integer_
      ),
      e01_employed = case_when(
        (e01_work_past_week == 1 | e03_job_or_business_past_week == 1) &
          e01_in_the_labor_force == 1 ~ 1L,
        e01_work_past_week == 2 & e03_job_or_business_past_week %in% c(2, 3) &
          e01_in_the_labor_force == 1 ~ 2L,
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
    select(-any_of(c(
      'section_a_to_e_line_number',
      'regular_hh_completed',
      'e01_nilf_1',
      'e01_nilf_2',
      'e01_nilf_3',
      'e01_nilf_emp',
      'e01_nilf_ofi',
      'e10_industry'
    )))
}

