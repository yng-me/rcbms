tidy_data_temp <- function(.record, .remove_geo_cols = T) {

  df <- parquet$hp |>
    tidy_cbms_data(.record, 'hp', .complete_cases = complete_cases) |>
    select_and_sort_columns(.refs = references)

  if(.remove_geo_cols) {
    df <- df |>
      select(
        -any_of(c('uuid', 'ean', 'bsn', 'husn', 'hsn')),
        -ends_with('_code')
      )
  }

  return(df)
}



result$hh_roster <- tidy_data_temp('section_a_to_e')

# result$hh_record <- tidy_data_temp('section_f', FALSE) |>
#   left_join(tidy_data_temp('section_k'), by = 'case_id') |>
#   left_join(tidy_data_temp('section_n'), by = 'case_id') |>
#   left_join(tidy_data_temp('section_m'), by = 'case_id') |>
#   left_join(tidy_data_temp('section_q'), by = 'case_id') |>
#   left_join(tidy_data_temp('section_r'), by = 'case_id')
