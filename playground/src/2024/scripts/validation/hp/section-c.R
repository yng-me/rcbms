# Filipino citizen indicator is missing or not in the value set

## Filipino citizen indicator should not be blank and should be from 1 to 3.

cv$cv_c_citizenship <- section_a_to_f %>%
  filter(!(c01_citizenship %in% c(1:3))) %>% 
  select_cv(c01_citizenship)




# Indicator for residence five years ago is missing or not in the value set

## Indicator for residence five years ago should not be blank and should be from 1 to 3 for household members five years old and over.

cv$cv_c_residence_five_years_ago <- section_a_to_f %>%
  filter(age >= 5, !(c02_resided_5_years_ago_reside %in% c(1:4))) %>% 
  select_cv(c02_resided_5_years_ago_reside)



### ELLA, INCLUDE OTHER CHECKS FOR PLACE OF RESIDENCE FIVE YEARS AGO 


# Overseas Filipino indicator is missing or not in the value set

## Overseas Filipino indicator should not be blank and should be from 1 to 7 for household members 15 years old and over.

cv$cv_c_ofi <- section_a_to_f %>%
  filter(age >= 15, !(c03_ofi %in% c(1:7))) %>% 
  select_cv(age, c03_ofi)



# With entry in overseas Filipino indicator but household member is less than 15 years old

## Overseas Filipino indicator should be blank for household members below 15 years old.

cv$cv_c_with_ofi <- section_a_to_f %>%
  filter(age < 15, !is.na(c03_ofi)) %>% 
  select_cv(age, c03_ofi)



# Internally displaced person indicator is missing or not in the value set

## Internally displaced person indicator should not be blank and should be 1 or 2 only.

cv$cv_c_internal_displacement <- section_a_to_f %>%
  filter(!(c04_internal_displacement %in% c(1:2))) %>% 
  select_cv(c04_internal_displacement)


# With responses in place of origin and moving date but household member is not an internally displaced person

## If the household member is not an internally displace person, details of movement should be blank (c05 and c06 should not have entries).

cv$cv_c_with_internal_displacement_details <- section_a_to_f %>%
  filter(c04_internal_displacement == 2) %>%
  filter_at(vars(matches('^c0[5-6]_.*')), any_vars(!is.na(.))) %>% 
  select_cv(c04_internal_displacement, matches('^c0[5-6]_.*'))


## TEMP CHECK SINCE DI PA OK ANG STRUCTURE

# With no responses in place of origin and moving date but household member is an internally displaced person

## If the household member is an internally displace person, details of movement (c05 and c06) should have valid entries.

cv$cv_c_internal_displacement_details_missing <- section_a_to_f %>%
  filter(c04_internal_displacement == 1) %>%
  filter_at(vars(matches('^c0[5-6]_.*')), any_vars(is.na(.))) %>% 
  select_cv(c04_internal_displacement, matches('^c0[5-6]_.*'))  
