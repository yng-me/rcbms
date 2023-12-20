tidy_cbms_data_temp <- function(.data, ...) {

  .data |>
    filter_and_select_regular_hh(prefix = 'r') |>
    mutate_at(
      vars(
        matches('^r\\d{2}'),
        -matches('_(other|specified)$'),
        -matches('^r17_tv_subscription$')
      ),
      as.integer) |>
    mutate_at(vars(matches('other|specified)$')), as.character) |>
    mutate(
      r07_floor_area_range = case_when(
        r07_floor_area < 10 ~ 1L,
        r07_floor_area < 30 ~ 2L,
        r07_floor_area < 50 ~ 3L,
        r07_floor_area < 80 ~ 4L,
        r07_floor_area < 120 ~ 5L,
        r07_floor_area < 150 ~ 6L,
        r07_floor_area < 200 ~ 7L,
        r07_floor_area >= 200 ~ 8L
      ),
      r10_year_constructed_range = case_when(
        r10_year_constructed >= 2020 ~ 1L,
        r10_year_constructed > 2010 ~ 2L,
        r10_year_constructed > 2000 ~ 3L,
        r10_year_constructed > 1990 ~ 4L,
        r10_year_constructed > 1980 ~ 5L,
        r10_year_constructed > 1970 ~ 6L,
        r10_year_constructed > 1960 ~ 7L,
        r10_year_constructed > 1950 ~ 8L,
        r10_year_constructed > 1940 ~ 9L,
        r10_year_constructed > 1930 ~ 10L,
        r10_year_constructed > 1920 ~ 11L,
        r10_year_constructed > 1910 ~ 12L,
        r10_year_constructed > 1900 ~ 13L,
        r10_year_constructed <= 1900 ~ 14L
      ),
      r15_fuel_for_cooking = if_else(
        r15_fuel_for_cooking %in% c(0, 7),
        9L,
        as.integer(r15_fuel_for_cooking)
      )
    ) |>
    select_and_sort_columns(...)
}
