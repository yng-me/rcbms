df_temp_tidy <- df_temp |> 
  filter(regular_hh_completed == 1) |> 
  select(-regular_hh_completed) |> 
  filter(!is.na(j_line_number)) |> 
  filter_at(vars(matches('^j\\d{2}')), any_vars(!is.na(.))) |> 
  mutate(
    j41_reason_not_availed_treatment_fct = if_else(
      j38_availed_medical_treatment == 1, 
      0L, 
      j41_reason_not_availed_treatment
    )
  ) |> 
  mutate(
    j17_disability_diagnosed_fct = factor(j17_disability_diagnosed, c(1, 2), c('Diagnosed by a Doctor', 'Not Diagnosed by a Doctor')),
    j24_rare_disease_diagnosed_fct = factor(j24_rare_disease_diagnosed, c(1, 2), c('Diagnosed by a Doctor', 'Not Diagnosed by a Doctor')),
    j25_type_of_rare_disease_fct = add_factor(j25_type_of_rare_disease, code_ref = 'j25'),
    j31_type_of_disability_in_pwd_id_fct = add_factor(j31_type_of_disability_in_pwd_id, code_ref = 'j37'),
    j37_type_of_sickness_fct = add_factor(j37_type_of_sickness, code_ref = 'j37'),
    j41_reason_not_availed_treatment_fct = add_factor(j41_reason_not_availed_treatment_fct, code_ref = 'j41')
  ) 
  