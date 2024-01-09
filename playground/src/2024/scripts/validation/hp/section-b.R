# Nuclear family assignment is missing or not in the value set

## Nuclear family assignment should not be blank and should be from 1 to 99.

cv$cv_b_nuclear_family <- section_a_to_f %>%
  filter(!(b01_nuclear_family %in% c(1:99))) %>% 
  select_cv(b01_nuclear_family)
  


# Relationship to nuclear family head is missing or not in the value set

## Relationship to nuclear family head should not be blank and should be from 1 to 10.

cv$cv_b_rel_to_nuc_fam_head <- section_a_to_f %>%
  filter(!(b02_relation_to_nuclear_family_head %in% c(1:10))) %>% 
  select_cv(b02_relation_to_nuclear_family_head)



# Household head but not in the first nuclear family and not the nuclear family head

## The household head should be the head of the first nuclear family.

cv$cv_hh_head_nuclear_fam <- section_a_to_f %>% 
  filter(a02_relation_to_hh_head == 1, b01_nuclear_family != 1 | b02_relation_to_nuclear_family_head > 1) %>% 
  select_cv(a02_relation_to_hh_head, b01_nuclear_family, b02_relation_to_nuclear_family_head)



# Relationship to nuclear family head is for male but sex is inconsistent

## If relationship to nuclear family head is son (4), brother (6), or father (8), then sex should be male. 

cv$cv_b_rel_to_nuc_fam_head_male <- section_a_to_f %>% 
  filter(b02_relation_to_nuclear_family_head %in% c(4, 6, 8), a03_sex != 1) %>% 
  select_cv(a03_sex, b02_relation_to_nuclear_family_head)



# Relationship to nuclear family head is for female but sex is inconsistent

## If relationship to nuclear family head is daughter (5), sister (7), or mother (9), then sex should be female. 

cv$cv_b_rel_to_nuc_fam_head_female <- section_a_to_f %>% 
  filter(b02_relation_to_nuclear_family_head %in% c(5, 7, 9), a03_sex != 2) %>% 
  select_cv(a03_sex, b02_relation_to_nuclear_family_head)



# Spouse of the nuclear family head but marital status is not married or common law/live in

## The marital status of the nuclear family head's spouse should either be married or common law/live in. Also, ensure that the nuclear family head and his/her spouse have the same marital status.

cv$cv_b_nuc_fam_spouse_marital_status <- section_a_to_f %>% 
  filter(b02_relation_to_nuclear_family_head == 2, !a07_marital_status %in% c(2:3)) %>% 
  select_cv(b02_relation_to_nuclear_family_head, a07_marital_status)



# Nuclear family head's spouse is below 10 years old

## Nuclear family head's spouse should not be below 10 years old. Verify and provide valid justification if necessary.

cv$cv_b_nuc_fam_spouse_age <- section_a_to_f %>% 
  filter((b02_relation_to_nuclear_family_head == 2), age < 10) %>% 
  select_cv(b02_relation_to_nuclear_family_head, age)



# Ethnicity of household member is missing or not in the value set

## Ethnicity should not be blank and should be valid.

cv$cv_b_ethnicity <- section_a_to_f %>% 
  filter(!(b03_ethnicity %in% c(1:291, 998, 999))) %>% 
  select_cv(b03_ethnicity)



# Other ethnicity is missing or not specified

## Answer in ethnicity is other. Specify other ethnicity.

cv$cv_b_missing_ethnicity_other <- section_a_to_f %>% 
  filter(b03_ethnicity == 999, is.na(b03_ethnicity_other)) %>% 
  select_cv(b03_ethnicity, b03_ethnicity_other)



# Responses for other ethnicity

## Answer in ethnicity is other. Check if it can be re-coded. If so, reflect the correct code and delete the answer in the specify field.   

cv$cv_b_ethnicity_other <- section_a_to_f %>% 
  filter(b03_ethnicity == 999, !is.na(b03_ethnicity_other)) %>% 
  select_cv(b03_ethnicity, b03_ethnicity_other)



# With entry in the specify field but ethnicity is not other

## Specify field should be blank if ethnicity is not other.


cv$cv_b_with_ethnicity_other <- section_a_to_f %>% 
  filter(b03_ethnicity != 999, !is.na(b03_ethnicity_other)) %>% 
  select_cv(b03_ethnicity, b03_ethnicity_other)



# Religious affiliation of household member is missing or not in the value set

## Religious affiliation should not be blank and should be valid.

cv$cv_b_religious_affiliation <- section_a_to_f %>% 
  filter(!(b04_religion %in% c(0:127, 998))) %>% 
  select_cv(b04_religion)



# Other religious affiliation is missing or not specified

## Answer in religious affiliation is other. Specify other religious affiliation.

cv$cv_b_missing_religious_affiliation_other <- section_a_to_f %>% 
  filter(b04_religion == 127, is.na(b04_religion_other)) %>% 
  select_cv(b04_religion, b04_religion_other)



# Responses for other religious affiliation

## Answer in religious affiliation is other. Check if it can be re-coded. If so, reflect the correct code and delete the answer in the specify field.

cv$cv_b_religious_affiliation_other <- section_a_to_f %>% 
  filter(b04_religion == 127, !is.na(b04_religion_other)) %>% 
  select_cv(b04_religion, b04_religion_other)



# With entry in the specify field but religious affiliation is not other

## Specify field should be blank if religious affiliation is not other.

cv$cv_b_with_religious_affiliation_other <- section_a_to_f %>% 
  filter(b04_religion != 127, !is.na(b04_religion_other)) %>% 
  select_cv(b04_religion, b04_religion_other)



# PhilID issuance status is missing or not in the value set

## PhilID issuance status should not be blank and should be 1, 2 or 8 only.

cv$cv_b_phil_id <- section_a_to_f %>%
  filter(!(b05_phil_id %in% c(1, 2, 8))) %>% 
  select_cv(b05_phil_id)



# PhilSys Card Number is missing or not valid

## PhilSys Card Number should not be blank and should be 16 digits if PhilID was issued to the household member (b05 = 1).

cv$cv_b_pcn <- section_a_to_f %>%
  filter(b05_phil_id == 1, is.na(b06_phil_id_pcn) | nchar(b06_phil_id_pcn) != 16)  %>% 
  select_cv(b05_phil_id, b06_phil_id_pcn)



# With response in PhilSys Card Number but PhilID was not issued to household member or status of issuance is unknown (b05 = 2, 8).

## If PhilID was not issued to household member or issuance status is unknown, PCN should be blank.

cv$cv_b_with_pcn <- section_a_to_f %>%
  filter(b05_phil_id %in% c(2, 8), !is.na(b06_phil_id_pcn))  %>% 
  select_cv(b05_phil_id, b06_phil_id_pcn)



# With responses in questions on solo parent but household member is below ten years old

## Household member below ten years old should not have responses in solo parents questions (b07 and b08)

cv$cv_b_with_solo_parent_items <- section_a_to_f %>%
  filter(age < 10, !is.na(b07_solo_parent) | !is.na(b08_solo_parent_id)) %>% 
  select_cv(age, b07_solo_parent, b08_solo_parent_id)



# Solo parent indicator is missing or not in the value set for household member 10 years old and over

## If household member is 10 years old and over, solo parent indicator should not be blank and should be 1, 2 or 8 only.

cv$cv_b_solo_parent <- section_a_to_f %>%
  filter(age >= 10, !(b07_solo_parent %in% c(1, 2, 8))) %>% 
  select_cv(age, b07_solo_parent)



# Ownership of solo parent ID is missing or not in the value set

## If household member is a solo parent, ownership of solo parent ID should not be blank and should be 1, 2 or 8 only.

cv$cv_b_solo_parent_id <- section_a_to_f %>%
  filter(age >= 10, b07_solo_parent == 1, !(b08_solo_parent_id %in% c(1, 2, 8))) %>% 
  select_cv(age, b07_solo_parent, b08_solo_parent_id)



# With response in ownership of solo parent ID but solo parent indicator is no or unknown (b07 = 2 or 8)

## If household member is not a solo parent or solo parent indicator is unknown, ownership of solo parent ID should be blank.

cv$cv_b_solo_parent_id <- section_a_to_f %>%
  filter(age >= 10, b07_solo_parent %in% c(2,8), !is.na(b08_solo_parent_id)) %>% 
  select_cv(age, b07_solo_parent, b08_solo_parent_id)



# Ownership of senior citizen ID is missing or not in the value set for household member 60 years old and over

## If household member is 60 years old and over, ownership of senior citizen ID should not be blank and should be 1, 2 or 8 only.

cv$cv_b_senior_citizen_id <- section_a_to_f %>%
  filter(age >= 60, !(b09_senior_citizen_id %in% c(1, 2, 8))) %>% 
  select_cv(age, b09_senior_citizen_id)



# With response in ownership of senior citizen ID but household member is below 60 years old

## If household member is below 60 years old, ownership of senior citizen ID should be blank.

cv$cv_b_with_senior_citizen_id <- section_a_to_f %>%
  filter(age < 60, !is.na(b09_senior_citizen_id)) %>% 
  select_cv(age, b09_senior_citizen_id)