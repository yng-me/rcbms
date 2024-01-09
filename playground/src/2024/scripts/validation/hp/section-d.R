# With responses in school attendance and its details (d01 to d05) but household member is less than three years old

## If household member is below three years old, items on school attendance should be blank.

cv$cv_d_with_school_attendance_items <- section_a_to_f %>%
  filter(age < 3) %>%  
  filter_at(vars(matches('^d0[1-5]_.*')), any_vars(!is.na(.))) %>%
  select_cv(age, matches('^d0[1-5]_.*'))



# School attendance is missing or not in the value set

## School attendance should not be blank and should be 1 or 2 only.

cv$cv_d_school_attendance <- section_a_to_f %>%
  filter(age >= 3, !(d01_currently_attending_school %in% c(1:2))) %>% 
  select_cv(age, d01_currently_attending_school)



# With responses in school location and type and current grade (d02 to d04) but household member is not attending school

## If household member is not attending school, items on type of school and current grade should be blank.

cv$cv_d_with_school_attendance <- section_a_to_f %>%
  filter(age >= 3, d01_currently_attending_school == 2) %>%
  filter_at(vars(matches('^d0[2-4]_.*')), any_vars(!is.na(.))) %>%
  select_cv(age, d01_currently_attending_school, matches('^d0[2-4]_.*'))




# School location is missing (ELLA, for inclusion yung value set)

## School location should not be blank and should be in the value set.

cv$cv_d_school_location <- section_a_to_f %>%
  filter(age >= 3, d01_currently_attending_school == 1, is.na(d02_school_address_within_the_country)| is.na(d02_school_address_outside_the_country)) %>% 
  select_cv(age, d01_currently_attending_school, d02_school_address_within_the_country, d02_school_address_outside_the_country)



# Type of school is missing or not in the value set

## Type of school should not be blank and should be 1 to 3 only.

cv$cv_d_school_type <- section_a_to_f %>%
  filter(age >= 3, d01_currently_attending_school == 1, !(d03_type_of_school_attended %in% c(1:3))) %>% 
  select_cv(age, d01_currently_attending_school, d03_type_of_school_attended)



# Current grade level is missing or not in the value set

## Current grade level should not be blank and should be valid.

cv$cv_d_current_grade_level <- section_a_to_f %>%
  filter(age >= 3, d01_currently_attending_school == 1, !(d04_current_grade_level %in% c(1:8))) %>% 
  select_cv(age, d01_currently_attending_school, d04_current_grade_level)

current_grade <- c(01000000, 02100000,
                   10000101, 10000102, 10000103, 10000104, 10000105, 10000106, 10000107, 10000201, 10000202, 10000203, 10000300,
                   20400101, 20400102, 20400103, 20400104, 
                   21400101, 21400102, 21400103, 20100104,
                   20400201, 20400202, 20400203, 20400300,
                   30400101, 30400102, 30400103, 30400104, 30400105, 30400106, 30400107, 30400108, 30400109, 30400110,
                   30400201, 30400202,
                   30400301, 30400302,
                   30500001, 30500002, 30500003, 30500004, 30500005, 30500006, 30500007, 30500008, 30500009, 30500010,
                   40000001, 40000002, 40000003,
                   50000001, 50000002, 50000003,
                   60000001, 60000002, 60000003, 60000004, 60000005, 60000006,
                   70000010,
                   80000010
                  )



# Current grade is missing or not in the value set

## Current grade should not be blank and should be valid.

cv$cv_d_current_grade <- section_a_to_f %>%
  filter(age >= 3, d01_currently_attending_school == 1, !(d04_current_grade_level %in% current_grade)) %>% 
  select_cv(age, d01_currently_attending_school, d04_current_grade_level)

# 
# 
# # Inconsistency between current grade and age
# 
# ## Di mo muna to sinama, Ella 
# ## Current grade is high
# 
# ## Ella, this is for HGC, minus one (either in age or grade level) for cur grade
# 
# cv$cv_current_grade_age <- section_a_to_f %>% 
#   filter(
#       (age <= 5 & d04_current_grade > 02100000) | 
#       (age == 6 & d04_current_grade > 11000101) |
#       (age == 7 & d04_current_grade > 11000102) |
#       (age == 8 & d04_current_grade > 11000103) |
#       (age == 9 & d04_current_grade > 11000104) |
#       (age == 10 & d04_current_grade > 11000105) |
#       (age == 11 & d04_current_grade > 11000106) |
#       (age == 12 & d04_current_grade > 21400101) |
#       (age == 13 & d04_current_grade > 21400102) |
#       (age == 14 & d04_current_grade > 21400103) |
#       (age == 15 & d04_current_grade > 21400104) |
#       (age == 16 & d04_current_grade >= 21400104) | ## 
#       (age == 17 & d04_current_grade > 60001) |
#       (age == 18 & d04_current_grade > 60002) |  
#       (age == 19 & d04_current_grade > 60003) |
#       (age == 20 & d04_current_grade > 60004) 
#   ) %>%
#   select_cv(age, c02_hgc, d04_current_grade)
# 
# 
# 
# 
# 
# # Current grade level is lower than or equal to HGC
# 
# ## Current grade should be higher than HGC. Provide justification for special cases.
# 
# cv$cv_d_current_grade_hgc <- section_a_to_f %>%
#   filter(age >= 3, d01_currently_attending_school == 1, a09_hgc >= d04_current_grade_level) %>% 
#   select_cv(age, d01_currently_attending_school, d04_current_grade_level, a09_hgc)
