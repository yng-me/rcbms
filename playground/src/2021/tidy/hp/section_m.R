tidy_cbms_data_temp <- function(.data) {

  .data |>
    mutate(
      m08_evacuated_temporarily = if_else(
        is.na(m08_evacuated_temporarily),
        8L,
        as.integer(m08_evacuated_temporarily)
      )
    )
}
