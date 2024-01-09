section_p <- parquet$hp$section_p %>% 
  create_case_id() %>% 
  collect()

## =========================================================================================== ##


# The main source of water supply is missing or not in the value set

## The main source of water supply should not be blank and should be 1 to 10 or 99 only.

cv$cv_p_water_supply_main_source <- section_p %>% 
  filter(!(p01_main_water %in% c(1:10, 99))) %>% 
  select_cv(p01_main_water)



# Other source of water supply is missing or not specified

## Answer in main source of water supply is other. Specify other source.

cv$cv_p_missing_water_supply_source_other <- section_p %>% 
  filter(p01_main_water == 99, is.na(p01_main_water_other)) %>% 
  select_cv(p01_main_water, p01_main_water_other)



# Responses for other source of water supply

## Answer in main source of water supply is other. Check if it can be re-coded. If so, reflect the correct code and delete the answer in the specify field.

cv$cv_p_water_supply_source_other <- section_p %>% 
  filter(p01_main_water == 99, !is.na(p01_main_water_other)) %>% 
  select_cv(p01_main_water, p01_main_water_other)



# With entry in the specify field but main source of water supply is not other

## Specify field should be blank if main source of water supply is not other.


cv$cv_p_with_water_supply_source_other <- section_p %>% 
  filter(p01_main_water != 99, !is.na(p01_main_water_other)) %>% 
  select_cv(p01_main_water, p01_main_water_other)



# The distance between house and main source of water supply is missing or not in the value set

## If the answer in p01 is code 2 to 8 or 99, the distance between house and main source of water supply should not be blank and should be valid.

cv$cv_p_water_supply_source_distance <- section_p %>% 
  filter(p01_main_water %in% c(2:88, 99), !p02_main_water_distance %in% c(1:9999)) %>% 
  select_cv(p01_main_water, p02_main_water_distance)


############### ELLA, MAGTANONG MUNA IKAW SA APIS TEAM WITH TITA RIZ RE YOUR QUERIES ##################

############### ELLA, MAGTANONG MUNA IKAW SA APIS TEAM WITH TITA RIZ RE YOUR QUERIES ##################

############### ELLA, MAGTANONG MUNA IKAW SA APIS TEAM WITH TITA RIZ RE YOUR QUERIES ##################

############### ELLA, MAGTANONG MUNA IKAW SA APIS TEAM WITH TITA RIZ RE YOUR QUERIES ##################

############### ELLA, MAGTANONG MUNA IKAW SA APIS TEAM WITH TITA RIZ RE YOUR QUERIES ##################


# The type of toilet facility is missing or not in the value set

## The type of toilet facility used by the household should not be blank and should be valid.

cv$cv_p_toilet_facility <- section_p %>% 
  filter(!(p09_toilet_facility %in% c(11:15, 21:23, 31, 41, 51, 71, 95, 99))) %>% 
  select_cv(p09_toilet_facility)

  
  
# Other type of toilet facility is missing or not specified

## Answer in type of toilet facility is other. Specify other type.

cv$cv_p_missing_toilet_facility_other <- section_p %>% 
  filter(p09_toilet_facility == 99, is.na(p09_other_toilet_facility_specified)) %>% 
  select_cv(p09_toilet_facility, p09_other_toilet_facility_specified)



# Responses for other type of toilet facility

## Answer in type of toilet facility is other. Check if it can be re-coded. If so, reflect the correct code and delete the answer in the specify field.   

cv$cv_p_toilet_facility_other <- section_p %>% 
  filter(p09_toilet_facility == 99, !is.na(p09_other_toilet_facility_specified)) %>% 
  select_cv(p09_toilet_facility, p09_other_toilet_facility_specified)



# With entry in the specify field but type of toilet facility is not other

## Specify field should be blank if type of toilet facility is not other.


cv$cv_p_with_toilet_facility_other <- section_p %>% 
  filter(p09_toilet_facility != 99, !is.na(p09_other_toilet_facility_specified)) %>% 
  select_cv(p09_toilet_facility, p09_other_toilet_facility_specified)



# With responses in items on details of toilet facility but the answer in type of toilet facility is public toilet or no facility/bush/field

## If answer in the type of toilet facility is public toilet or no facility/bush/field (code 71 or 95), details on toilet facility should be blank.

cv$cv_p_with_toilet_facility_items <- section_p %>% 
  filter(p09_toilet_facility %in% c(71, 95)) %>%
  filter_at(vars(matches('^p1[0-2]_.*')), any_vars(!is.na(.))) %>%
  select_cv(p09_toilet_facility, matches('^p1[0-2]_.*'))



# Location of toilet facility is missing or not in the value set

## If answer in the type of toilet facility is codes 11 to 15, 21 to 23, 31, 41, 51 or 99, location of toilet facility should not be blank and should be 1 to 3 only.

  cv$cv_p_toilet_facility_location <- section_p %>% 
  filter(p09_toilet_facility %in% c(11:15, 21:23, 31, 41, 51, 99), !p10_toilet_facility_located %in% c(1:3)) %>%
  select_cv(p09_toilet_facility, p10_toilet_facility_located)



# Sharing of toilet facility is missing or not in the value set

## If answer in the type of toilet facility is codes 11 to 15, 21 to 23, 31, 41, 51 or 99, sharing of toilet facility should not be blank and should either be 1 or 2 only.

cv$cv_p_toilet_facility_sharing <- section_p %>% 
  filter(p09_toilet_facility %in% c(11:15, 21:23, 31, 41, 51, 99), !p11_share_toilet %in% c(1, 2)) %>%
  select_cv(p09_toilet_facility, p11_share_toilet)



# General public's access to toilet facility is missing or not in the value set

## If answer in the type of toilet facility is codes 11 to 15, 21 to 23, 31, 41, 51 or 99 and the toilet facility is shared with other people (p11 = 1), general public's access to toilet facility should not be blank and should either be 1 or 2 only.

cv$cv_p_toilet_facility_public_access <- section_p %>% 
  filter(p09_toilet_facility %in% c(11:15, 21:23, 31, 41, 51, 99), p11_share_toilet == 1, !p12_toilet_for_public_use %in% c(1, 2)) %>%
  select_cv(p09_toilet_facility, p11_share_toilet, p12_toilet_for_public_use)



# With entry in general public's access to toilet facility but toilet facility is not shared with others

## If answer in the type of toilet facility is codes 11 to 15, 21 to 23, 31, 41, 51 or 99 and the toilet facility is not shared with others  (p11 = 2), general public's access to toilet facility should be blank.

cv$cv_p_with_toilet_facility_public_access <- section_p %>% 
  filter(p09_toilet_facility %in% c(11:15, 21:23, 31, 41, 51, 99), p11_share_toilet == 2, !is.na(p12_toilet_for_public_use)) %>%
  select_cv(p09_toilet_facility, p11_share_toilet, p12_toilet_for_public_use)



# Ways of disposing garbage is missing or invalid

## Ways of disposing garbage should not be blank and should be A to I and Z only. 

cv$cv_p_waste_disposal <- section_p %>% 
  filter(p13_waste_disposal == '' |
        !grepl('[a-iz]', p13_waste_disposal, ignore.case = T)) %>% 
  select_cv(p13_waste_disposal)



# Other way of disposing garbage is missing or not specified

## Answer in way of disposing garbage is other. Specify other way of disposing garbage.

cv$cv_p_missing_waste_disposal_other <- section_p %>% 
  filter(grepl('[z]', p13_waste_disposal, ignore.case = T),
         is.na(p13_other_waste_disposal_specified)) %>% 
  select_cv(p13_waste_disposal, p13_other_waste_disposal_specified)



# Responses for other way of disposing garbage

## Answer in way of disposing garbage is other. Check if it can be re-coded. If so, reflect the correct code and delete the answer in the specify field.

cv$cv_p_waste_disposal_other <- section_p %>% 
  filter(grepl('[z]', p13_waste_disposal, ignore.case = T),
         !is.na(p13_other_waste_disposal_specified)) %>% 
  select_cv(p13_waste_disposal, p13_other_waste_disposal_specified)



# With entry in the specify field but way of disposing garbage is not other

## Specify field should be blank if way of disposing garbage is not other.


cv$cv_p_with_waste_disposal_other <- section_p %>% 
  filter(!grepl('[z]', p13_waste_disposal, ignore.case = T),
         !is.na(p13_other_waste_disposal_specified)) %>% 
  select_cv(p13_waste_disposal, p13_other_waste_disposal_specified)



# Handwashing facility observation is missing or not in the value set

## Handwashing facility observation should not be blank and should be 1 to 5 or 9 only. 

cv$cv_p_handwashing_facility <- section_p %>% 
  filter(!p14_handwashing %in% c(1:5, 9)) %>%
  select_cv(p14_handwashing)



# Other place for hand washing is missing or not specified

## Answer in place for hand washing is other. Specify other place.

cv$cv_p_missing_handwashing_other <- section_p %>% 
  filter(p14_handwashing == 9, is.na(p15_handwashing_other)) %>% 
  select_cv(p14_handwashing, p15_handwashing_other)



# Responses for other place for hand washing

## Answer in place for hand washing is other. Check if it can be re-coded. If so, reflect the correct code and delete the answer in the specify field.

cv$cv_p_handwashing_other <- section_p %>% 
  filter(p14_handwashing == 9, !is.na(p15_handwashing_other)) %>% 
  select_cv(p14_handwashing, p15_handwashing_other)



# With entry in the specify field but place for hand washing is not other

## Specify field should be blank if place for hand washing is not other.


cv$cv_p_with_handwashing_other <- section_p %>% 
  filter(p14_handwashing != 9, !is.na(p15_handwashing_other)) %>% 
  select_cv(p14_handwashing, p15_handwashing_other)



# With responses in details on place for handwashing but there was no handwashing facility observed

## If handwashing facility was not observed (p14 is 4, 5 or 9), details on place for handwashing should be blank.

cv$cv_p_with_handwashing_items <- section_p %>% 
  filter(p14_handwashing %in% c(4, 5, 9)) %>%
  filter_at(vars(matches('^p(1[5-7])_.*')), any_vars(!is.na(.))) %>%
  select_cv(p14_handwashing, matches('^p(1[5-7])_.*'))



# Presence of water at the place for hand washing is missing or not in the value set

## If handwashing facility was observed (p14 is 1, 2, or 3), presence of water at the place for hand washing should not be blank and should be 1 to 5 or 9 only.

cv$cv_p_handwashing_water_presence <- section_p %>% 
  filter(p14_handwashing %in% c(1:3), !p15_handwashing_with_water %in% c(1, 2)) %>%
  select_cv(p14_handwashing, p15_handwashing_with_water)



# Presence of soap at the place for hand washing is missing or not in the value set

## If handwashing facility was observed (p14 is 1, 2, or 3), presence of soap at the place for hand washing should not be blank and should be 1 to 5 or 9 only.

cv$cv_p_handwashing_soap_presence <- section_p %>% 
  filter(p14_handwashing %in% c(1:3), !p16_handwashing_with_soap %in% c(1, 2)) %>%
  select_cv(p14_handwashing, p16_handwashing_with_soap)



# With entry in type of soap or other handwashing agent but soap or other handwashing agent was not observed at the place for handwashing

## If soap or other handwashing agent was not observed at the place for handwashing (p15 = 2), type of soap or other handwashing agent should be blank.


cv$cv_p_with_soap_type <- section_p %>% 
  filter(p14_handwashing %in% c(1:3), p16_handwashing_with_soap == 2, !is.na(p17_type_of_soap_or_detergent)) %>%
  select_cv(p14_handwashing, p16_handwashing_with_soap, p17_type_of_soap_or_detergent)



# Type of soap or other handwashing agent is missing or not in the value set

## If soap or other handwashing agent was observed at the place for handwashing (p15 = 1), type of soap or other handwashing agent should not be blank and should be A to B and Z only.


cv$cv_p_soap_type <- section_p %>% 
  filter(p14_handwashing %in% c(1:3), p16_handwashing_with_soap == 1, 
         !grepl('[a-cz]', p17_type_of_soap_or_detergent, ignore.case = T)) %>% 
  select_cv(p14_handwashing, p16_handwashing_with_soap, p17_type_of_soap_or_detergent)



# Other type of handwashing agent is missing or not specified

## Answer in type of handwashing agent is other. Specify other type of handwashing agent.

cv$cv_p_missing_soap_type_other <- section_p %>% 
  filter(p14_handwashing %in% c(1:3), p16_handwashing_with_soap == 1,
         grepl('[z]', p17_type_of_soap_or_detergent, ignore.case = T),
         is.na(p17_type_of_soap_or_detergent_other)) %>% 
  select_cv(p14_handwashing, p16_handwashing_with_soap, p17_type_of_soap_or_detergent, p17_type_of_soap_or_detergent_other)



# Responses for other type of handwashing agent present at the place for handwashing

## Answer in type of handwashing agent present at the place for handwashing is other. Check if it can be re-coded. If so, reflect the correct code and delete the answer in the specify field.

cv$cv_p_soap_type_other <- section_p %>% 
  filter(p14_handwashing %in% c(1:3), p16_handwashing_with_soap == 1, 
         grepl('[z]', p17_type_of_soap_or_detergent, ignore.case = T),
         !is.na(p17_type_of_soap_or_detergent_other)) %>% 
  select_cv(p14_handwashing, p16_handwashing_with_soap, p17_type_of_soap_or_detergent, p17_type_of_soap_or_detergent_other)



# With entry in the specify field but type of soap or handwashing agent present at the place for handwashing is not other

## Specify field should be blank if type of soap or handwashing agent present at the place for handwashing is not other.


cv$cv_p_with_soap_type_other <- section_p %>% 
  filter(p14_handwashing %in% c(1:3), p16_handwashing_with_soap == 1, 
         !grepl('[z]', p17_type_of_soap_or_detergent, ignore.case = T),
         !is.na(p17_type_of_soap_or_detergent_other)) %>% 
  select_cv(p14_handwashing, p16_handwashing_with_soap, p17_type_of_soap_or_detergent, p17_type_of_soap_or_detergent_other)