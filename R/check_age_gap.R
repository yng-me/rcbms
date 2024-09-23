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
  .ref_code = 'relation_to_hh_head',
  .config = getOption('rcbms.config')
) {

  var <- .config$project$hp$variable
  rel_var <- var$relation_to_hh_head
  age_var <- var$age_computed
  sex_var <- var$sex

  primary_member <- paste0('_', tolower(get_ref_code(.ref_code, get_label = T))[.primary_member])
  primary_member_var <- paste0(age_var, primary_member)

  relation_to_primary_member <- paste0('_', tolower(get_ref_code(.ref_code, get_label = T))[.relation_to_primary_member])
  relation_to_primary_member_var <- paste0(age_var, relation_to_primary_member)

  .data |>
    dplyr::filter(!!as.name(rel_var) == .primary_member) |>
    dplyr::select(case_id, line_number, dplyr::any_of(c(rel_var, age_var, sex_var))) |>
    dplyr::left_join(
      .data |>
        dplyr::filter(!!as.name(rel_var) == .relation_to_primary_member) |>
        dplyr::select(case_id, dplyr::any_of(c(rel_var, age_var, sex_var))),
      by = 'case_id',
      suffix = c(primary_member, relation_to_primary_member)
    ) |>
    dplyr::filter(abs(!!as.name(relation_to_primary_member_var) - !!as.name(primary_member_var)) < .threshold) |>
    select_cv(dplyr::matches(c(rel_var, age_var, sex_var)))
}

