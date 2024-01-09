section_n <- parquet$hp$section_n %>% 
  create_case_id() %>% 
  collect()


## =========================================================================================== ##


# Respondent's perception on safety is missing or not in the value set

## Respondent's perception on safety should not be blank and should be 1 to 5, or 8 only. 

cv$cv_n_safety <- section_n %>% 
  filter(!(n01_safe_walking_alone %in% c(1:5, 8))) %>% 
  select_cv(n01_safe_walking_alone)



# Crime victimization is missing or not in the value set

## Crime victimization should not be blank and should be 1 or 2 only. 

cv$cv_n_crime_victimization <- section_n %>% 
  filter(!(n02_victim_of_crime %in% c(1, 2))) %>% 
  select_cv(n02_victim_of_crime)



# Setting of the crime is missing or not in the value set

## If the household became victim of crime, setting of the crime should not be blank and should be 1 to 4 only. 

cv$cv_n_crime_setting <- section_n %>% 
  filter(n02_victim_of_crime == 1, !(n03_location_of_crime %in% c(1, 4))) %>% 
  select_cv(n02_victim_of_crime, n03_location_of_crime)



# With entry in setting of the crime but household did not become victim of crime

## If the household did not become victim of crime, setting of the crime should be blank. 

cv$cv_n_with_crime_setting <- section_n %>% 
  filter(n02_victim_of_crime == 2, !is.na(n03_location_of_crime)) %>% 
  select_cv(n02_victim_of_crime, n03_location_of_crime)
