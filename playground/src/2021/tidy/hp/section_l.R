df_temp_tidy <- df_temp |> 
  filter(regular_hh_completed == 1) |> 
  select(-regular_hh_completed) |> 
  filter(!is.na(l_line_number)) |> 
  filter_at(vars(matches('^l\\d{2}')), any_vars(!is.na(.))) |> 
  mutate(
    l06_main_agri_activity_fct = add_factor(l06_main_agri_activity, code_ref = 'l06'),
    l07_a_growing_of_crops_engagement_fct = add_factor(l07_a_growing_of_crops_engagement, code_ref = 'l07'),
    l07_b_livestock_engagement_fct = add_factor(l07_b_livestock_engagement, code_ref = 'l07')
  )