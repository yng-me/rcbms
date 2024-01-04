df_temp_tidy <- df_temp |> 
  pivot_longer_lno() |>
  filter_and_select_regular_hh(prefix = 'd') |> 
  mutate_if(is.double, as.integer) |> 
  mutate_at(vars(matches('^d(08|12)_.*_work_[2-6]$')), as.integer) |> 
  mutate_at(vars(matches('^d(08|12)_.*_specified$')), as.character) |> 
  mutate(
    line_number = as.integer(line_number),
    d01_voter = case_when(
      d01_registered_voter == 1 & d02_voted_last_elections == 1 ~ 1L,
      d01_registered_voter == 1 & d02_voted_last_elections != 1 ~ 2L,
      d01_registered_voter == 2 ~ 3L
    )
  ) |> 
  create_case_id() |>
  distinct(case_id, line_number, .keep_all = T) |> 
  left_join(from_b06_ofi, by = c('case_id', 'line_number')) |> 
  left_join(from_section_a, c('case_id', 'line_number')) |>
  select_with_geo(line_number, sex, age, everything(), -case_id) 