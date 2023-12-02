#' Add age groups
#'
#' @param .data
#' @param age
#' @param prefix
#'
#' @return
#' @export
#'
#' @examples
add_age_groups <- function(.data, age, prefix = 'a07') {
  .data <- .data |>
    dplyr::mutate(
      add_age_group_five_year = dplyr::case_when(
        {{age}} < 5 ~ 1,
        {{age}} < 10 ~ 2,
        {{age}} < 15 ~ 3,
        {{age}} < 20 ~ 4,
        {{age}} < 25 ~ 5,
        {{age}} < 30 ~ 6,
        {{age}} < 35 ~ 7,
        {{age}} < 40 ~ 8,
        {{age}} < 45 ~ 9,
        {{age}} < 50 ~ 10,
        {{age}} < 55 ~ 11,
        {{age}} < 60 ~ 12,
        {{age}} < 65 ~ 13,
        {{age}} < 70 ~ 14,
        {{age}} < 75 ~ 15,
        {{age}} < 80 ~ 16,
        {{age}} < 85 ~ 17,
        {{age}} < 90 ~ 18,
        {{age}} < 95 ~ 19,
        {{age}} < 100 ~ 20,
        {{age}} >= 100 ~ 21
      ),
      add_age_group_15_and_over = dplyr::case_when(
        {{age}} < 15 ~ 1,
        {{age}} < 25 ~ 2,
        {{age}} < 35 ~ 3,
        {{age}} < 45 ~ 4,
        {{age}} < 55 ~ 5,
        {{age}} < 65 ~ 6,
        {{age}} >= 65 ~ 7
      ),
      add_age_group_fertility = dplyr::case_when(
        {{age}} < 15 ~ 1,
        {{age}} < 20 ~ 2,
        {{age}} < 25 ~ 3,
        {{age}} < 30 ~ 4,
        {{age}} < 35 ~ 5,
        {{age}} < 40 ~ 6,
        {{age}} < 45 ~ 7,
        {{age}} < 50 ~ 8,
        {{age}} >= 50 ~ 9
      ),
      add_age_group_schooling = dplyr::case_when(
        {{age}} >= 3 & {{age}} < 5 ~ 1,
        {{age}} == 5 ~ 2,
        {{age}} < 12 ~ 3,
        {{age}} < 16 ~ 4,
        {{age}} < 18 ~ 5,
        {{age}} < 21 ~ 6,
        {{age}} >= 21 & {{age}} <= 24 ~ 7
      )
    ) |>
    dplyr::rename_with(
      ~ paste0(
        prefix,
        '_',
        stringr::str_remove(., '^add_')
      ),
      starts_with('add_age_group_')
    )

  attr(.data[[paste0(prefix, '_age_group_five_year')]], 'valueset') <- data.frame(
    value = 1L:21L,
    label = c(
      '< 5',
      '5-9',
      '10-14',
      '15-19',
      '20-24',
      '25-29',
      '30-34',
      '35-39',
      '40-44',
      '45-49',
      '50-54',
      '55-59',
      '60-64',
      '65-69',
      '70-74',
      '75-79',
      '80-84',
      '85-89',
      '90-94',
      '95-99',
      '100+'
    )
  )

  return(.data)
}
