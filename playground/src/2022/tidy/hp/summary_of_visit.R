if(config$data_chunk_options$conform_line_number == F) {
  df_temp_tidy <- df_temp |> 
    mutate(result_of_visit = as.integer(result_of_visit))
} else {
  df_temp_tidy <- df_temp |> 
    mutate(result_of_visit = as.integer(result_of_visit)) 
}

df_temp_tidy <- df_temp |> 
  create_case_id(filter_completed = F) |> 
  change_status_of_incomplete_cases(new_status_code = 7L) |> 
  select(-case_id) |> 
  mutate_at(vars(matches('_(other|specified)$')), as.character) |>
  mutate(
    number_of_visits = as.integer(number_of_visits),
    hh_size = as.integer(hh_size),
    number_of_males = as.integer(number_of_males),
    number_of_females = as.integer(number_of_females),
    number_of_nuclear_family = as.integer(number_of_nuclear_family),
    agree_to_sign_the_waiver = as.integer(agree_to_sign_the_waiver),
    reason_not_signing_the_waiver = as.integer(reason_not_signing_the_waiver),
    hh_geotagged = as.integer(hh_geotagged)
  ) |> 
  select_with_geo(result_of_visit, everything())
