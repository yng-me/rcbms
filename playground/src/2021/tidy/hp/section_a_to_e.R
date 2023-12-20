tidy_cbms_data_temp <- function(.data, ...) {

  .data |>
    mutate(
      line_number = as.integer(line_number),
      line_number = if_else(
        case_id != lag(case_id) & is.na(line_number),
        1L,
        line_number
      ),
      a07_age = as.integer(a07_age),
      a09_marital_status = if_else(is.na(a09_marital_status) & a07_age < 15, 1L, a09_marital_status),
      a12_phil_id_ownership = case_when(
        a12_phil_id == 1 ~ 1L,
        a12_phil_id %in% c(2, 8) & a12_phil_id_step_2 == 1 ~ 2L,
        a12_phil_id %in% c(2, 8) & a12_phil_id_step_2 == 2 ~ 3L
      ),
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
        c02_hgc > 80010 ~ 17L # college graduate or higher
      ),
      d01_voter = case_when(
        d01_registered_voter == 1 & d02_voted_last_elections == 1 ~ 1L,
        d01_registered_voter == 1 & d02_voted_last_elections != 1 ~ 2L,
        d01_registered_voter == 2 ~ 3L
      ),
      d04_barangay_assembly = if_else(
        d04_reason_not_attended_brgy_assembly == 9 |
          is.na(d04_reason_not_attended_brgy_assembly),
        as.integer(d04_reason_not_attended_brgy_assembly),
        as.integer(d04_reason_not_attended_brgy_assembly) + 1L
      ),
      e01_nilf_emp = e01_work_past_week == 2 & e03_job_or_business_past_week %in% c(2, 3),
      e01_nilf_1 = e01_nilf_emp & e22_try_look_for_work_or_do_business == 1 & e24_available_for_work == 2,
      e01_nilf_2 = e01_nilf_emp & e22_try_look_for_work_or_do_business == 2 & e22_reason_not_looking_for_work %in% c(6:10),
      e01_nilf_3 = e01_nilf_emp & e22_try_look_for_work_or_do_business == 2 &
        e22_reason_not_looking_for_work %in% c(0:5) & e24_available_for_work == 2,
      e01_nilf_ofi = b06_ofi %in% c(1:3, 6),
      e01_in_the_labor_force = if_else(e01_nilf_1 | e01_nilf_2 | e01_nilf_3 | e01_nilf_ofi, 2L, 1L, NA_integer_),
      e01_employed = case_when(
        (e01_work_past_week == 1 | e03_job_or_business_past_week == 1) & e01_in_the_labor_force == 1 ~ 1L,
        e01_work_past_week == 2 & e03_job_or_business_past_week %in% c(2, 3) & e01_in_the_labor_force == 1 ~ 2L,
        TRUE ~ NA_integer_
      ),
      e08_occupation_group = if_else(e08_psoc == 0, NA_character_, str_pad(e08_psoc, width = 4, pad = '0')),
      e08_occupation_group = if_else(
        grepl('^0[1-3]', e08_occupation_group),
        10L,
        as.integer(str_sub(e08_occupation_group, 1, 1))
      ),
      e10_industry_group = case_when(
        e10_psic > 0 & e10_psic < 330  ~ 1L,
        e10_psic < 3500 ~ 2L,
        e10_psic < 9910 ~ 3L,
        TRUE ~ NA_integer_
      ),
      e10_industry_group = as.integer(str_sub(e10_industry_group, 1, 2))
    ) |>
    convert_to_na() |>
    add_age_groups(a07_age, .prefix = 'a07') |>
    add_ip_group(a10_ethnicity, .prefix = 'a10') |>
    add_wgss(.prefix = 'a17') |>
    mutate_line_number() |>
    select_and_sort_columns(...)
}
