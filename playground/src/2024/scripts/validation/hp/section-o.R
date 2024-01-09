section_o <- parquet$hp$section_o %>% 
  create_case_id() %>% 
  collect()


section_o_cm <- parquet$hp$section_o_cm %>% 
  create_case_id() %>% 
  collect()


## =========================================================================================== ##

# Household's membership to social or health insurance programs is missing or not in the value set

## Household's membership to social or health insurance programs should not be blank and should either be 1 or 2 only.

cv$cv_o_insurance_programs <- section_o %>%
  filter_at(vars(matches('o01_[a-d]_.*')), any_vars(!(. %in% c(1, 2)))) %>% 
  select_cv(matches('o01_[a-d]_.*'))

### 
### ELLA, 'yung structure nung O02 na record ay magulo, di s'ya naka per line number tapos yes no lang sa mga vars ### 
### 

# Household's involvement or inclusion to  social assistance programs is missing or not in the value set

## Household's involvement or inclusion to  social assistance programs should not be blank and should either be 1 or 2 only.

cv$cv_o_social_assistance_programs <- section_o %>%
  filter_at(vars(matches('o03_[a-p]_.*')), any_vars(!(. %in% c(1, 2)))) %>% 
  select_cv(matches('o03_[a-p]_.*'))



# Household is not involved or included in 4Ps - Regular Conditional Cash Transfer but with details on the receipt of assistance or benefits  

## If household is not involved or included in 4Ps - Regular Conditional Cash Transfer, details on the availment or receipt of benefits or assistance (o04 - o05) should be blank.  

cv$cv_o_with_4ps_rcct_details <- section_o %>%
  filter(o03_a_4ps_regular == 2) %>% 
  filter_at(vars(matches('o0[4-5]_a_.*')), any_vars(!is.na(.))) %>% 
  select_cv(o03_a_4ps_regular, matches('o0[4-5]_a_.*'))



# Household is not involved or included in 4Ps - Modified Conditional Cash Transfer but with details on the receipt of assistance or benefits  

## If household is not involved or included in 4Ps - Modified Conditional Cash Transfer, details on the availment or receipt of benefits or assistance (o04 - o05) should be blank.  

cv$cv_o_with_4ps_mcct_details <- section_o %>%
  filter(o03_b_4ps_modified == 2) %>% 
  filter_at(vars(matches('o0[4-5]_b_.*')), any_vars(!is.na(.))) %>% 
  select_cv(o03_b_4ps_modified, matches('o0[4-5]_b_.*'))



# Household is not involved or included in SPISC or SocPen but with details on the receipt of assistance or benefits  

## If household is not involved or included in SPISC or SocPen, details on the availment or receipt of benefits or assistance (o04 - o05) should be blank.  

cv$cv_o_with_socpen_details <- section_o %>%
  filter(o03_c_social_pension_senior_citizen == 2) %>% 
  filter_at(vars(matches('o0[4-5]_c_.*')), any_vars(!is.na(.))) %>% 
  select_cv(o03_c_social_pension_senior_citizen, matches('o0[4-5]_c_.*'))



# Household is not involved or included in KALAHI-CIDSS but with details on the receipt of assistance or benefits  

## If household is not involved or included in KALAHI-CIDSS, details on the availment or receipt of benefits or assistance (o04 - o05) should be blank.  

cv$cv_o_with_kalahi_cidss_details <- section_o %>%
  filter(o03_d_kalahi_cidss == 2) %>% 
  filter_at(vars(matches('o0[4-5]_d_.*')), any_vars(!is.na(.))) %>% 
  select_cv(o03_d_kalahi_cidss, matches('o0[4-5]_d_.*'))



# Household is not involved or included in StuFAP but with details on the receipt of assistance or benefits  

## If household is not involved or included in StuFAP, details on the availment or receipt of benefits or assistance (o04 - o05) should be blank.  

cv$cv_o_with_stufap_details <- section_o %>%
  filter(o03_e_stufap == 2) %>% 
  filter_at(vars(matches('o0[4-5]_e_.*')), any_vars(!is.na(.))) %>% 
  select_cv(o03_e_stufap, matches('o0[4-5]_e_.*'))



# Household is not involved or included in SHS VP but with details on the receipt of assistance or benefits  

## If household is not involved or included in SHS VP, details on the availment or receipt of benefits or assistance (o04 - o05) should be blank.  

cv$cv_o_with_shs_vp_details <- section_o %>%
  filter(o03_f_shs_vp == 2) %>% 
  filter_at(vars(matches('o0[4-5]_f_.*')), any_vars(!is.na(.))) %>% 
  select_cv(o03_f_shs_vp, matches('o0[4-5]_f_.*'))



# Household is not involved or included in Food Stamp Program but with details on the receipt of assistance or benefits  

## If household is not involved or included in Food Stamp Program, details on the availment or receipt of benefits or assistance (o04 - o05) should be blank.  

cv$cv_o_with_food_stamp_details <- section_o %>%
  filter(o03_f_shs_vp == 2) %>% 
  filter_at(vars(matches('o0[4-5]_f_.*')), any_vars(!is.na(.))) %>% 
  select_cv(o03_f_shs_vp, matches('o0[4-5]_f_.*'))



# Household is not involved or included in Disaster Assistance Program but with details on the receipt of assistance or benefits  

## If household is not involved or included in Disaster Assistance Program, details on the availment or receipt of benefits or assistance (o04 - o05) should be blank.  

cv$cv_o_with_disaser_assistance_details <- section_o %>%
  filter(o03_f_shs_vp == 2) %>% 
  filter_at(vars(matches('o0[4-5]_f_.*')), any_vars(!is.na(.))) %>% 
  select_cv(o03_f_shs_vp, matches('o0[4-5]_f_.*'))