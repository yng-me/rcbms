section_a <- parquet$hp$section_a |>
  collect() |>
  create_case_id()
