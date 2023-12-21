tidy_cbms_data_temp <- function(.data) {

  .data |>
    mutate(
      f05_entrep_psic = as.integer(f05_entrep_psic),
      f05_industry_group = case_when(
        f05_entrep_psic %in% c(1111:1300, 1511:1582) ~ 'A',
        f05_entrep_psic %in% c(1411:1499, 1561:1566) ~ 'B',
        f05_entrep_psic %in% 3111:3299 ~ 'C',
        f05_entrep_psic %in% 1701:2400 ~ 'D',
        f05_entrep_psic %in% 5100:9900 ~ 'E',
        f05_entrep_psic %in% 10110:33200 ~ 'F',
        f05_entrep_psic %in% 35101:35300 ~ 'G',
        f05_entrep_psic %in% 36000:39000 ~ 'H',
        f05_entrep_psic %in% 41001:43900 ~ 'I',
        f05_entrep_psic %in% 45101:47999 ~ 'J',
        f05_entrep_psic %in% 49111:52299 ~ 'K',
        f05_entrep_psic %in% 53100:53209 ~ 'L',
        f05_entrep_psic %in% 55101:56309 ~ 'M',
        f05_entrep_psic %in% 58110:63990 ~ 'N',
        f05_entrep_psic %in% 64110:66300 ~ 'O',
        f05_entrep_psic %in% 68110:68200 ~ 'P',
        f05_entrep_psic %in% 69100:75000 ~ 'Q',
        f05_entrep_psic %in% 85111:85602 ~ 'R',
        f05_entrep_psic %in% 86111:88909 ~ 'S',
        f05_entrep_psic %in% 77100:82990 ~ 'T',
        f05_entrep_psic %in% 90001:93299 ~ 'U',
        f05_entrep_psic %in% c(84111:84300, 94110:99090) ~ 'V'
      ),
      f06_online_presence = case_when(
        f06_entrep_use_ecommerce == 1 & f07_entrep_use_social_media == 1 ~ 1L,
        f06_entrep_use_ecommerce == 1 & f07_entrep_use_social_media != 1 ~ 2L,
        f06_entrep_use_ecommerce != 1 & f07_entrep_use_social_media == 1 ~ 3L,
        f06_entrep_use_ecommerce != 1 & f07_entrep_use_social_media != 1 ~ 4L,
        TRUE ~ NA_integer_
      ),
      f08_entrep_year_started_range = case_when(
        f08_entrep_year_started >= 2020 ~ 1L,
        f08_entrep_year_started >= 2015 ~ 2L,
        f08_entrep_year_started >= 2010 ~ 3L,
        f08_entrep_year_started >= 2000 ~ 4L,
        f08_entrep_year_started >= 1990 ~ 5L,
        f08_entrep_year_started >= 1980 ~ 6L,
        f08_entrep_year_started >= 1970 ~ 7L,
        f08_entrep_year_started >= 1950 ~ 8L,
        f08_entrep_year_started < 1950 ~ 9L
      ),
      f10_a_number_of_working_owners = if_else(
        is.na(f10_a_number_of_working_owners),
        0L,
        f10_a_number_of_working_owners
      ),
      f10_b_number_of_paid_employees = if_else(
        is.na(f10_b_number_of_paid_employees),
        0L,
        f10_b_number_of_paid_employees
      ),
      f10_c_total_employees = f10_a_number_of_working_owners + f10_b_number_of_paid_employees
    )
}
