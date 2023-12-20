result$hh_roster <- parquet$hp |>
  tidy_cbms_data(
    .record = 'section_a_to_e',
    .input_data = 'hp',
    .refs = references,
    .complete_cases = complete_cases
  )
