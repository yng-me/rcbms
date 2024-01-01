tidy_cbms_data_temp <- function(.data) {
  .data |>
    filter_at(vars(matches('^k0[1-8]_')), any_vars(!is.na(.))) |>
    mutate_at(vars(matches('^k0[1-8]_')), list(new = ~ if_else(. == 1L, 1L, 0L, NA_integer_))) |>
    mutate(k10_raw_score = rowSums(select(., matches('^k0[1-8]_.*_new$')), na.rm = T)) |>
    rename_at(vars(matches('^k0[1-8]_.*_new$')), ~ paste0('k09_', letters[as.integer(str_sub(., 3, 3))]))
}
