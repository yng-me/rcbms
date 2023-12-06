#' Add age groups
#'
#' @param .data
#' @param .age
#' @param .prefix
#'
#' @return
#' @export
#'
#' @examples
add_age_groups <- function(.data, .age, .prefix = 'a07') {
  .data <- .data |>
    dplyr::mutate(
      add_age_group_five_year = dplyr::case_when(
        {{.age}} < 5 ~ 1L,
        {{.age}} < 10 ~ 2L,
        {{.age}} < 15 ~ 3L,
        {{.age}} < 20 ~ 4L,
        {{.age}} < 25 ~ 5L,
        {{.age}} < 30 ~ 6L,
        {{.age}} < 35 ~ 7L,
        {{.age}} < 40 ~ 8L,
        {{.age}} < 45 ~ 9L,
        {{.age}} < 50 ~ 10L,
        {{.age}} < 55 ~ 11L,
        {{.age}} < 60 ~ 12L,
        {{.age}} < 65 ~ 13L,
        {{.age}} < 70 ~ 14L,
        {{.age}} < 75 ~ 15L,
        {{.age}} < 80 ~ 16L,
        {{.age}} < 85 ~ 17L,
        {{.age}} < 90 ~ 18L,
        {{.age}} < 95 ~ 19L,
        {{.age}} < 100 ~ 20L,
        {{.age}} >= 100 ~ 21L
      ),
      add_age_group_hh_head = dplyr::case_when(
        {{.age}} < 15 ~ 1L,
        {{.age}} < 25 ~ 2L,
        {{.age}} < 35 ~ 3L,
        {{.age}} < 45 ~ 4L,
        {{.age}} < 55 ~ 5L,
        {{.age}} < 65 ~ 6L,
        {{.age}} >= 65 ~ 7L
      ),
      add_age_group_fertility = dplyr::case_when(
        {{.age}} < 15 ~ 1L,
        {{.age}} < 20 ~ 2L,
        {{.age}} < 25 ~ 3L,
        {{.age}} < 30 ~ 4L,
        {{.age}} < 35 ~ 5L,
        {{.age}} < 40 ~ 6L,
        {{.age}} < 45 ~ 7L,
        {{.age}} < 50 ~ 8L,
        {{.age}} >= 50 ~ 9L
      ),
      add_age_group_schooling = dplyr::case_when(
        {{.age}} >= 3 & {{.age}} < 5 ~ 1L,
        {{.age}} == 5 ~ 2L,
        {{.age}} < 12 ~ 3L,
        {{.age}} < 16 ~ 4L,
        {{.age}} < 18 ~ 5L,
        {{.age}} < 21 ~ 6L,
        {{.age}} >= 21 & {{.age}} <= 24 ~ 7L
      )
    ) |>
    dplyr::rename_with(
      ~ paste0(
        prefix,
        '_age_',
        stringr::str_remove(., '^add_age_')
      ),
      dplyr::starts_with('add_age_group_')
    )

  attr(.data[[paste0(prefix, '_age_group_five_year')]], 'label') <- 'Five-year age group'
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

  attr(.data[[paste0(prefix, '_age_group_hh_head')]], 'label') <- 'Age group'
  attr(.data[[paste0(prefix, '_age_group_hh_head')]], 'valueset') <- data.frame(
    value = 1L:7L,
    label = c(
      'Less than 15',
      '15-24',
      '25-34',
      '35-44',
      '45-54',
      '55-64',
      '65 and over'
    )
  )

  attr(.data[[paste0(prefix, '_age_group_fertility')]], 'label') <- 'Age group'
  attr(.data[[paste0(prefix, '_age_group_fertility')]], 'valueset') <- data.frame(
    value = 1L:9L,
    label = c(
      'Less than 15',
      '15-19',
      '20-24',
      '25-29',
      '30-34',
      '35-39',
      '40-44',
      '45-49',
      '50 and over'
    )
  )

  attr(.data[[paste0(prefix, '_age_group_schooling')]], 'label') <- 'Age group'
  attr(.data[[paste0(prefix, '_age_group_schooling')]], 'valueset') <- data.frame(
    value = 1L:7L,
    label = c(
      '3-4 years old',
      '5 years old',
      '6-11 years old',
      '12-15 years old',
      '16-17 years old',
      '18-20 years old',
      '21-24 years old'
    )
  )

  return(.data)
}
