tidy_cbms_data_temp <- function(.data) {
  .data |>
    rename(line_number = d12_line_number) |>
    filter(!is.na(line_number))
}
