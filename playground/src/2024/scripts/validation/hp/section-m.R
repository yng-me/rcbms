section_m <- parquet$hp$section_m %>% 
  create_case_id() %>% 
  collect()


## =========================================================================================== ##


# Access to internet is missing or not in the value set

## Access to internet should not be blank and should be 1 or 2 only. 

cv$cv_m_internet_access <- section_m %>% 
  filter(!(m01_internet_access %in% c(1, 2))) %>% 
  select_cv(m01_internet_access)



# With responses in internet connection details but household has no access to internet

## If the household has no access to internet, there should be no details of internet access (m02 to m04 should be blank)

cv$cv_m_with_internet_access_items <- section_m %>% 
  filter(m01_internet_access == 2, !is.na(m02_location_of_internet_access) | !is.na(m03_internet_at_home) | !is.na(m04_type_of_internet_connection_at_home)) %>%
  select_cv(m01_internet_access, m02_location_of_internet_access, m03_internet_at_home, m04_type_of_internet_connection_at_home)



# Place where household members access the internet is missing or invalid

## If the household has access to internet, place where household members access the internet should not be blank and should be A to G and Z only. 

cv$cv_m_internet_access_location <- section_m %>% 
  filter(m01_internet_access == 1, 
         m02_location_of_internet_access == '' |
               !grepl('[a-gz]', m02_location_of_internet_access, ignore.case = T)) %>% 
  select_cv(m01_internet_access, m02_location_of_internet_access)



# Other place where household members access the internet is missing or not specified

## Answer in place where household members access the internet is other. Specify other place where household members access the internet.

cv$cv_m_missing_internet_access_location_other <- section_m %>% 
  filter(m01_internet_access == 1, 
         grepl('[z]', m02_location_of_internet_access, ignore.case = T),
         is.na(m02_location_of_internet_access_other)) %>% 
  select_cv(m01_internet_access, m02_location_of_internet_access, m02_location_of_internet_access_other)



# Responses for other places where household members access the internet

## Answer in place where household members access the internet is other. Check if it can be re-coded. If so, reflect the correct code and delete the answer in the specify field.

cv$cv_m_internet_access_location_other <- section_m %>% 
  filter(m01_internet_access == 1, 
         grepl('[z]', m02_location_of_internet_access, ignore.case = T),
         !is.na(m02_location_of_internet_access_other)) %>% 
  select_cv(m01_internet_access, m02_location_of_internet_access, m02_location_of_internet_access_other)



# With entry in the specify field but place where household members access the internet is not other

## Specify field should be blank if place where household members access the internet is not other.


cv$cv_m_with_internet_access_location_other <- section_m %>% 
  filter(m01_internet_access == 1, 
         !grepl('[z]', m02_location_of_internet_access, ignore.case = T),
         !is.na(m02_location_of_internet_access_other)) %>% 
  select_cv(m01_internet_access, m02_location_of_internet_access, m02_location_of_internet_access_other)



# With responses in details of internet connection at home but household has no access to internet at home

## If the household has no access to internet at home, there should be no details of internet connection at home (m03 to m04 should be blank).


cv$cv_m_with_internet_at_home_items <- section_m %>% 
  filter(m01_internet_access == 1,
         !grepl('[a]', m02_location_of_internet_access, ignore.case = T),
         !is.na(m03_internet_at_home) | !is.na(m04_type_of_internet_connection_at_home)) %>% 
  select_cv(m01_internet_access, m02_location_of_internet_access, m03_internet_at_home, m04_type_of_internet_connection_at_home)



# Availability of own internet at home is missing or not in the value set

## If the household has access to internet at home, availability of own internet at home should not be blank and should be 1 or 2 only.

cv$cv_m_internet_at_home <- section_m %>% 
  filter(m01_internet_access == 1,
         grepl('[a]', m02_location_of_internet_access, ignore.case = T),
         !(m03_internet_at_home %in% c(1,2))) %>% 
  select_cv(m01_internet_access, m02_location_of_internet_access, m03_internet_at_home)



# Type of internet connection at home is missing or invalid

## If the household has access to internet and has its own internet at home, type of internet connection at home should not be blank and should be A to D only.

cv$cv_m_internet_type <- section_m %>% 
  filter(m01_internet_access == 1,
         grepl('[a]', m02_location_of_internet_access, ignore.case = T),
         m03_internet_at_home == 1,
         !grepl('[a-d]', m04_type_of_internet_connection_at_home, ignore.case = T)) %>% 
  select_cv(m01_internet_access, m02_location_of_internet_access, m03_internet_at_home, m04_type_of_internet_connection_at_home)



# With entry in type of internet connection at home

## If the household does not have its own internet at home, type of internet connection should be blank.

cv$cv_m_with_internet_type <- section_m %>% 
  filter(m01_internet_access == 1,
         grepl('[a]', m02_location_of_internet_access, ignore.case = T),
         m03_internet_at_home == 2,
         !is.na(m04_type_of_internet_connection_at_home)) %>% 
  select_cv(m01_internet_access, m02_location_of_internet_access, m03_internet_at_home, m04_type_of_internet_connection_at_home)


