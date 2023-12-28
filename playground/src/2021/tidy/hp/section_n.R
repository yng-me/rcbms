tidy_cbms_data_temp <- function(.data) {
  .data |>
    mutate(
      n01_internet_access = case_when(
        n01_internet_access == 1 & n03_internet_at_home == 1 ~ 1L,
        n01_internet_access == 1 & n03_internet_at_home == 2 ~ 2L,
        n01_internet_access == 2 ~ 3L
      )
    )
}
