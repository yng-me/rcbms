df_temp_tidy <- df_temp |>
  filter_and_select_regular_hh(prefix = 'g') |>
  mutate_at(
    vars(
      matches('^g\\d{2}'),
      -matches('_(other|specified)$'),
      -matches('^g18_other_agri_org_\\d{2}$'),
      -matches('area$'),
      -matches('^g38')
    ),
    as.integer
  ) |>
  mutate_at(vars(matches('_(other|specified)$')), as.character) |>
  mutate_at(vars(matches('^g18_other_agri_org_\\d{2}$')), as.character) |>
  mutate_at(vars(matches('(^g38|area$)')), as.double)
