df_temp <- df_temp |>
  filter_at(vars(matches('^p\\d{2}')), any_vars(!is.na(.))) |> 
  filter(regular_hh_completed == 1) |> 
  select(-regular_hh_completed) |> 
  mutate(
    p08_feeding_program_fct = factor(
      p08_feeding_program, 
      c(1, 2), 
      c(
        'Received assistance from any government feeding program',	
        'Did not receive assistance from any government feeding program'
      )
    )
  )
  