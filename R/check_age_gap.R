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

  pattern <- paste0(primary_member, '|', relation_to_primary_member, '$')
  cols <- c('line_number', rel_var, age_var, sex_var)

  .data |>
    dplyr::filter(!!as.name(rel_var) == .primary_member) |>
    dplyr::select(case_id, dplyr::any_of(cols)) |>
    dplyr::collect() |>
    dplyr::mutate(line_number = as.integer(line_number)) |>
    dplyr::left_join(
      .data |>
        dplyr::mutate(line_number = as.integer(line_number)) |>
        dplyr::filter(!!as.name(rel_var) == .relation_to_primary_member) |>
        dplyr::select(case_id, dplyr::any_of(cols)) |>
        dplyr::collect(),
      by = 'case_id',
      suffix = c(primary_member, relation_to_primary_member)
    ) |>
    dplyr::filter(abs(!!as.name(relation_to_primary_member_var) - !!as.name(primary_member_var)) < .threshold) |>
    tidyr::pivot_longer(cols = dplyr::matches(cols)) |>
    dplyr::mutate(name = stringr::str_remove_all(name, pattern)) |>
    tidyr::pivot_wider(
      names_from = name,
      values_from = value,
      values_fn = list
    ) |>
    tidyr::unnest(dplyr::everything()) |>
    dplyr::group_by(case_id) |>
    tidyr::nest() |>
    dplyr::mutate(
      data = purrr::map_chr(data, \(x) {
        x |>
          dplyr::mutate(case_id, .before = 1) |>
          dplyr::mutate_all(as.character) |>
          jsonlite::toJSON() |>
          as.character()
      })
    ) |>
    select_cv(data)
}

