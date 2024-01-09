section_j <- parquet$hp$section_j %>% 
  create_case_id() %>% 
  collect()


## =========================================================================================== ##

# Experience of being worried about not having enough food to eat is missing or not in the value set

## Experience of being worried about not having enough food to eat should not be blank and should be 1, 2, 8 or 9 only.


cv$cv_j_worried <- section_j %>% 
  filter(!(j01_worried %in% c(1, 2, 8, 9))) %>% 
  select_cv(j01_worried)



# Experience of being unable to eat healthy and nutritious food is missing or not in the value set

## Experience of being unable to eat healthy and nutritious food should not be blank and should be 1, 2, 8 or 9 only.


cv$cv_j_unable_to_eat_healthy <- section_j %>% 
  filter(!(j02_not_eaten_healthy %in% c(1, 2, 8, 9))) %>% 
  select_cv(j02_not_eaten_healthy)




# Experience of eating only a few kinds of food is missing or not in the value set

## Experience of eating only a few kinds of food should not be blank and should be 1, 2, 8 or 9 only.

cv$cv_j_ate_few_kinds_of_food <- section_j %>% 
  filter(!(j03_ate_few_food %in% c(1, 2, 8, 9))) %>% 
  select_cv(j03_ate_few_food)



# Experience of skipping a meal is missing or not in the value set

## Experience of skipping a meal should not be blank and should be 1, 2, 8 or 9 only.

cv$cv_j_skipped_a_meal <- section_j %>% 
  filter(!(j04_skipped_meal %in% c(1, 2, 8, 9))) %>% 
  select_cv(j04_skipped_meal)



# Experience of eating less than what should be is missing or not in the value set

## Experience of eating less than what should be should not be blank and should be 1, 2, 8 or 9 only.

cv$cv_j_ate_less <- section_j %>% 
  filter(!(j05_ate_less %in% c(1, 2, 8, 9))) %>% 
  select_cv(j05_ate_less)




# Experience of running out of food is missing or not in the value set

## Experience of running out of food should not be blank and should be 1, 2, 8 or 9 only.

cv$cv_j_ran_out_of_food <- section_j %>% 
  filter(!(j06_ran_out_of_food %in% c(1, 2, 8, 9))) %>% 
  select_cv(j06_ran_out_of_food)



# Experience of being hungry but did not eat is missing or not in the value set

## Experience of being hungry but did not eat should not be blank and should be 1, 2, 8 or 9 only.

cv$cv_j_hungry_but_did_not_eat <- section_j %>% 
  filter(!(j07_hungry %in% c(1, 2, 8, 9))) %>% 
  select_cv(j07_hungry)




# Experience of not eating for a whole day is missing or not in the value set

## Experience of not eating for a whole day should not be blank and should be 1, 2, 8 or 9 only.

cv$cv_j_did_not_eat_for_a_day <- section_j %>% 
  filter(!(j08_not_eaten_whole_day %in% c(1, 2, 8, 9))) %>% 
  select_cv(j08_not_eaten_whole_day)