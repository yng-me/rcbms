section_q <- parquet$hp$section_q %>% 
  create_case_id() %>% 
  collect()


## =========================================================================================== ##


# The type of building that the household occupied is missing or not in the value set

## The type of building that the household occupied should not be blank and should be 1 to 5, or 8 only. 

cv$cv_q_building_type <- section_q %>% 
  filter(!(q01_building_type %in% c(1:10))) %>% 
  select_cv(q01_building_type)



# With responses in items on housing characteristics but building type is none (code 8) or temporary evacuation center/ relocation area (code 10) 

## If answer in the type of building is either none (code 8) or temporary evacuation center/ relocation area (code 10), housing characteristics items should be blank.

cv$cv_q_with_housing_characteristics <- section_q %>% 
  filter(q01_building_type %in% c(8, 10)) %>% 
  filter_at(vars(matches('^q(0[2-9]|[10-13])_.*')), any_vars(!is.na(.))) %>%
  select_cv(q01_building_type, matches('^q(0[2-9]|[10-13])_.*'))



# With responses in some items on housing characteristics (q02 to q09) but building type is other (code 9) 

## If answer in the type of building is other (code 9), some housing characteristics items (q02 to q09) should be blank.

cv$cv_q_with_some_housing_characteristics <- section_q %>% 
  filter(q01_building_type == 9) %>% 
  filter_at(vars(matches('^q0[2-9]_.*')), any_vars(!is.na(.))) %>%
  select_cv(q01_building_type, matches('^q0[2-9]_.*'))



# The number of floors of the building that the household occupied is missing or not in the value set

## If the answer in the type of building is code 1 to 7, the number of floors of the building that the household occupied should not be blank and should be from 1 to 99 only. 

cv$cv_q_floor_number <- section_q %>% 
  filter(q01_building_type %in% c(1:7), !q02_number_of_floors %in% c(1:99)) %>% 
  select_cv(q01_building_type, q02_number_of_floors)



# The main construction material of the roof is missing or not in the value set

## If the answer in the type of building is code 1 to 7, the main construction material of the roof of the building that the household occupied should not be blank and should be from 1 to 7 or 9 only. 

cv$cv_q_roof <- section_q %>% 
  filter(q01_building_type %in% c(1:7), !q03_roof %in% c(1:7, 9)) %>% 
  select_cv(q01_building_type, q03_roof)




# The main construction material of the outer walls is missing or not in the value set

## If the answer in the type of building is code 1 to 7, the main construction material of the outer walls of the building that the household occupied should not be blank and should be from 1 to 12 or 99 only. 

cv$cv_q_outer_walls <- section_q %>% 
  filter(q01_building_type %in% c(1:7), !q04_outer_walls %in% c(1:12, 99)) %>% 
  select_cv(q01_building_type, q04_outer_walls)




# The main construction material of the floor is missing or not in the value set

## If the answer in the type of building is code 1 to 7, the main construction material of the floor of the building that the household occupied should not be blank and should be from 1 to 6 or 9 only. 

cv$cv_q_floor <- section_q %>% 
  filter(q01_building_type %in% c(1:7), !q05_floor %in% c(1:6, 9)) %>% 
  select_cv(q01_building_type, q05_floor)



# The estimated floor area is missing or not in the value set

## If the answer in the type of building is code 1 to 7, the estimated floor area of the housing unit that the household occupied should not be blank and should be valid. 

cv$cv_q_estimated_floor_area <- section_q %>% 
  filter(q01_building_type %in% c(1:7), !q06_floor_area %in% c(1:9999)) %>% ## re-visit this ella, check specs
  select_cv(q01_building_type, q06_floor_area)



# The number of bedrooms is missing or not in the value set

## If the answer in the type of building is code 1 to 7, the number of bedrooms of the housing unit that the household occupied should not be blank and should be valid. 

cv$cv_q_bedroom_number <- section_q %>% 
  filter(q01_building_type %in% c(1:7), !q07_number_of_bedrooms %in% c(1:99)) %>% 
  select_cv(q01_building_type, q07_number_of_bedrooms)



# The tenure status of the housing unit and lot is missing or not in the value set

## If the answer in the type of building is code 1 to 7, the tenure status of the housing unit and lot that the household occupied should not be blank and should be from 1 to 7 only. 

cv$cv_q_housing_unit_lot_tenure_status <- section_q %>% 
  filter(q01_building_type %in% c(1:7), !q08_tenure %in% c(1:7)) %>% 
  select_cv(q01_building_type, q08_tenure)



# The year of construction of the housing unit is missing or not in the value set

## If the answer in the type of building is code 1 to 7, the year of construction of the housing unit that the household occupied should not be blank and should be valid. 

cv$cv_q_construction_year <- section_q %>% 
  filter(q01_building_type %in% c(1:7), !q09_year_constructed %in% c(1950:2024)) %>% 
  select_cv(q01_building_type, q09_year_constructed)



# Availability of electricity is missing or not in the value set

## If the answer in the type of building is code 1 to 7 or 9, availability of electricity in the dwelling place should not be blank and should be either 1 or 2 only.

cv$cv_q_electricity <- section_q %>% 
  filter(q01_building_type %in% c(1:7, 9), !q10_electricity %in% c(1, 2)) %>% 
  select_cv(q01_building_type, q10_electricity)



# Type of fuel used for lighting is missing or not in the value set

## If the answer in the type of building is code 1 to 7 or 9, the type of fuel used by the household for lighting should not be blank and should be from 1 to 6 or 9 only. 

cv$cv_q_lighting_fuel <- section_q %>% 
  filter(q01_building_type %in% c(1:7, 9), q10_electricity == 1, !q11_fuel_for_lighting %in% c(1:6, 9)) %>% 
  select_cv(q01_building_type, q10_electricity, q11_fuel_for_lighting)



# With entry in fuel used for lighting but electricity is not available in the dwelling place

## If electricity is not available in the dwelling place, the type of fuel used for lighting should be blank.

cv$cv_q_with_lighting_fuel <- section_q %>% 
  filter(q01_building_type %in% c(1:7, 9), q10_electricity == 2, !is.na(q11_fuel_for_lighting)) %>% 
  select_cv(q01_building_type, q10_electricity, q11_fuel_for_lighting)



# Type of fuel used for cooking is missing or not in the value set

## If the answer in the type of building is code 1 to 7 or 9, the type of fuel used by the household for cooking should not be blank and should be from 1 to 6 or 9 only.

cv$cv_q_cooking_fuel <- section_q %>% 
  filter(q01_building_type %in% c(1:7, 9), !q12_fuel_for_cooking %in% c(1:6, 9)) %>% 
  select_cv(q01_building_type, q12_fuel_for_cooking)



# Household used electricity for lighting or cooking but electricity is not available in the dwelling place

## If electricity is not available in the household's place of dwelling (q10 = 2), the household should not be able to use electricity in lighting and cooking.


cv$cv_q_electricity_lighting_cooking <- section_q %>% 
  filter(q01_building_type %in% c(1:7, 9), q10_electricity == 2, q11_fuel_for_lighting == 1 | q12_fuel_for_cooking == 1) %>% 
  select_cv(q01_building_type, q12_fuel_for_cooking)



# Ownership of household conveniences is missing or not in the value set

## Ownership of household conveniences should not be blank and should be valid.

cv$cv_q_household_conveniences <- section_q %>%
  filter(q01_building_type %in% c(1:7, 9)) %>% 
  filter_at(vars(matches('q13_[a-v]_.*')), any_vars(!(. %in% c(0:99)))) %>% 
  select_cv(q01_building_type, matches('q13_[a-v]_.*'))
