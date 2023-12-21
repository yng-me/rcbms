tidy_cbms_data_temp <- function(.data) {

  .data |>
    mutate_at(vars(
      matches('^j0[1-8]_')),
      list(new = ~ if_else(. %in% c(1, 2), ., NA_integer_, NA_integer_))
    ) |>
    mutate_at(vars(matches('^j0[1-8]_')), ~ if_else(. == 2, 0L, .)) |>
    mutate(j10_raw_score = rowSums(select(., matches('^j0[1-9]_.*_new$')), na.rm = T)) |>
    mutate(j10_raw_score = as.integer(j10_raw_score)) |>
    rename_at(
      vars(matches('^j0[1-8]_.*_new$')),
      ~ paste0('j11_', letters[as.integer(str_sub(., 3, 3))])
    )

}
