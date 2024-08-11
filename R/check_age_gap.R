#' Title
#'
#' @param .data
#' @param .primary_member
#' @param .relation_to_primary_member
#' @param .threshold
#' @param .config
#'
#' @return
#' @export
#'
#' @examples

check_age_gap <- function(
  .data,
  .primary_member,
  .relation_to_primary_member,
  .threshold = 10,
  .config = getOption('rcbms.config')
) {

  var <- .config$project$hp$variable
  rel_var <- var$relation_to_hh_head
  age_var <- var$age_computed
  sex_var <- var$sex

  .data |>
    dplyr::filter(!!as.name(rel_var) %in% c(.primary_member, .relation_to_primary_member)) |>
    dplyr::add_count(case_id) |>
    dplyr::filter(n > 1) |>
    dplyr::select(case_id, line_number, dplyr::all_of(c(rel_var, age_var, sex_var))) |>
    dplyr::group_by(case_id) |>
    tidyr::nest() |>
    dplyr::mutate(
      data = purrr::map_chr(data, \(x) {
        age_rel <- x |>
          dplyr::filter(!!as.name(rel_var) == .primary_member) |>
          pull(age_var)
        x |>
          dplyr::mutate(age_of_primary_member = age_rel[1]) |>
          dplyr::filter(
            !!as.name(rel_var) == .relation_to_primary_member,
            abs(age_of_primary_member - !!as.name(age_var)) < .threshold
          ) |>
          jsonlite::toJSON() |>
          as.character()
      })
    ) |>
    dplyr::filter(data != "[]") |>
    select_cv(data)
}
