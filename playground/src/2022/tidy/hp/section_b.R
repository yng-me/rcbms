df_temp_tidy <- df_temp |> 
  pivot_longer_lno() |>
  filter_and_select_regular_hh(prefix = 'b') |>
  mutate_at(vars(matches('^b\\d{2}'), -matches('other$')), as.integer) |> 
  mutate_at(vars(matches('other$')), as.character) |> 
  create_case_id() |>
  mutate(
    line_number = as.integer(line_number),
    b01_migrant_life_time = if_else(b01_mother_resided > 1, 1L, 2L, NA_integer_),
    b02_migrant_5_years = if_else(b02_resided_5_years_ago > 1, 1L, 2L, NA_integer_),
    b04_migrant_6_months = if_else(b04_resided_6_months_ago > 1, 1L, 2L, NA_integer_)
  ) |> 
  add_migration('b01_migrant_type_life_time', b01_mother_resided, b01_mother_resided_within_country) |> 
  add_migration('b01_migrant_type_5_years', b02_resided_5_years_ago, b02_resided_5_years_ago_within_the_country) |> 
  add_migration('b01_migrant_type_6_months', b04_resided_6_months_ago, b04_resided_6_months_ago_within_the_country) |> 
  left_join(from_section_a, c('case_id', 'line_number')) |>
  select_with_geo(line_number, sex, age, everything())

from_b06_ofi <- df_temp |> 
  create_case_id() |> 
  select(case_id, line_number, b06_ofi) |> 
  distinct(case_id, line_number, .keep_all = T)


clear_objects(add_migration)