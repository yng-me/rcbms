tidy_cbms_data_temp <- function(.data) {
  .data %>%
    mutate_at(vars(
      matches('^f03_[a-v]_.*')),
      list(new = ~ if_else(. %in% c(1, 2), ., NA_integer_, NA_integer_))
    ) %>%
    mutate_at(vars(matches('^f03_[a-v]_.*_new$')), ~ if_else(. == 2, 0L, .)) %>%
    mutate(f03_number_of_entrep = rowSums(select(., matches('^f03_[a-v]_.*_new$')), na.rm = T)) %>%
    mutate(f03_with_entrep = if_else(f03_number_of_entrep > 0, 1L, 2L)) %>%
    select(-ends_with('_new'))
}
