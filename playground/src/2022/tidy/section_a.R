df_temp <- df_temp %>% 
  filter_at(vars(matches('^a\\d.*'), -matches(c('^a13_pcn$', 'a01_.*'))), any_vars(!is.na(.))) %>%
  filter_and_select_regular_hh(prefix = 'a') %>% 
  mutate(
    a02_relation_to_hh_head = as.integer(a02_relation_to_hh_head),
    a03_nuclear_family = as.integer(a03_nuclear_family),
    a04_relation_to_nuclear_family_head = as.integer(a04_relation_to_nuclear_family_head),
    age = convert_age(mdy(a06_birthday), to = config$project$ref_period),
    line_number = as.integer(line_number),
    a10_ethnicity_other = as.character(a10_ethnicity_other),
    a11_religion_other = as.character(a11_religion_other),
    a13_pcn = as.character(a13_pcn),
    a16_lgu_id_number = as.character(a16_lgu_id_number)
  ) %>%
  create_case_id() %>%
  mutate(
    a09_marital_status = if_else(is.na(a09_marital_status) & age < 15, 1L, a09_marital_status),
    a12_phil_id_ownership = case_when(
      a12_phil_id == 1 ~ 1L,
      a12_phil_id %in% c(2, 8) & a14_phil_id_step_2 == 1 ~ 2L,
      a12_phil_id == 2 ~ 3L,
      a12_phil_id == 8 | a14_phil_id_step_2 == 8 ~ 8L
    )
  ) %>% 
  filter(age >= 0) %>% 
  add_age_groups() %>% 
  add_wgss(prefix = 'a20') %>% 
  mutate_if(is.double, as.integer) %>% 
  mutate(
    a06_birthday = mdy(a06_birthday),
    sex = factor(as.integer(a05_sex), c(1, 2), c('Male', 'Female'))
  )

from_section_a <- df_temp %>%
  distinct(case_id, line_number, .keep_all = T) %>%
  select(case_id, line_number, age, sex)

df_temp <- df_temp %>%
  select_with_geo(age, everything(), -case_id)

suppressWarnings(rm(refs_ip, get_max))
  
