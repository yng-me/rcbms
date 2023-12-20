complete_cases <- parquet |>
  get_complete_cases(!is.na(a05_sex), as.integer(a07_age) >= 0)
