df_temp <- df_temp |> 
  pivot_longer_lno() |>
  filter_and_select_regular_hh(prefix = 'c') |>
  create_case_id() |> 
  mutate_at(vars(matches('^c0[1-8]'), -matches('other$')), as.integer) |> 
  mutate_at(vars(matches('other$')), as.character) |> 
  mutate(line_number = as.integer(line_number)) |> 
  mutate_at(vars(matches('^c09.*_course_[1-5]$')), as.integer) |> 
  mutate_at(vars(matches('^c09.*_other_[1-5]$')), as.character) |> 
  left_join(from_section_a, c('case_id', 'line_number')) |> 
  mutate(
    c02_hgc_group = case_when(
      c02_hgc <= 2000 ~ 1L, # no grade completed / early childhood education,
      c02_hgc <= 10017 ~ 2L, # elementary level,
      c02_hgc == 10018 ~ 3L, # elementary graduate,
      c02_hgc <= 24014 ~ 4L, # junior high school level,
      c02_hgc <= 24024 ~ 5L, # junior high school graduate,
      c02_hgc %in% c(34011, 34012, 34021, 34022, 34031, 34032, 35011, 35012) ~ 6L,
      c02_hgc %in% c(34013:34018, 34023:34028, 34033, 35013:35018) ~ 7L, # senior high school graduate,
      c02_hgc %in% c(40001:40003) ~ 8L, # post-secondary non-tertiary level,
      c02_hgc %in% c(40011:49999) ~ 9L, # post-secondary non-tertiary graduate,
      c02_hgc %in% c(50001:50003) ~ 10L, # short-cycle tertiary level,
      c02_hgc %in% c(50011:59999) ~ 11L, # short-cycle tertiary graduate,
      c02_hgc %in% c(60001:60006) ~ 12L, # college level,
      c02_hgc %in% c(69998:69999) ~ 13L, # college graduate,
      c02_hgc == 70010 ~ 14L, # masteral level,
      c02_hgc %in% c(79998:79999) ~ 15L, # masteral graduate,
      c02_hgc == 80010 ~ 16L, # doctoral level,
      c02_hgc > 80010 ~17L # college graduate or higher
    )
  ) |> 
  select_with_geo(line_number, sex, age, everything(), -case_id) 