#' Title
#'
#' @param .data
#' @param .relation
#' @param .config
#'
#' @return
#' @export
#'
#' @examples
#'
check_duplicate_relation <- function(
  .data,
  .relation,
  .config = getOption("rcbms.config")
) {

  var <- .config$project$hp$variable
  relation_to_hh_head_var <- var$relation_to_hh_head
  line_number_var <- var$line_number

  .data |>
    dplyr::select(
      case_id,
      dplyr::matches('(_geo|_agg|_code)$'),
      dplyr::any_of(c('line_number', line_number_var)),
      dplyr::any_of(c('a02_relation_to_hh_head', relation_to_hh_head_var))
    ) |>
    dplyr::group_by(case_id, dplyr::pick(dplyr::matches('(_geo|_agg|_code)$'))) |>
    tidyr::nest() |>
    dplyr::mutate(
      data = purrr::map(data, \(x) {
        matched <- x |>
          dplyr::filter(!!as.name(relation_to_hh_head_var) == as.integer(.relation))

        if(nrow(matched) > 1) {

          if('line_number' %in% names(matched)) {
            lno <- paste(stringr::str_pad(matched$line_number, width = 2, pad = '0'), collapse = ', ')
          } else if(line_number_var %in% names(matched)) {
            lno <- paste(stringr::str_pad(matched[line_number_var], width = 2, pad = '0'), collapse = ', ')
          } else {
            lno <- NA_character_
          }

          if('a02_relation_to_hh_head' %in% names(matched)) {
            rel <- paste(stringr::str_pad(matched$a02_relation_to_hh_head, width = 2, pad = '0'), collapse = ', ')
          } else if(relation_to_hh_head_var %in% names(matched)) {
            rel <- paste(stringr::str_pad(matched[relation_to_hh_head_var], width = 2, pad = '0'), collapse = ', ')
          } else {
            rel <- NA_character_
          }

          x |>
            filter(!!as.name(relation_to_hh_head_var) == 1)|>
            dplyr::mutate(
              line_numbers = lno,
              relations_to_hh_head = rel
            )

        } else {
          x |>
            dplyr::mutate(
              line_numbers = NA_character_,
              relations_to_hh_head = NA_character_
            ) |>
            dplyr::filter_all(dplyr::all_vars(is.na(.)))
        }

      })
    ) |>
    tidyr::unnest(data) |>
    dplyr::ungroup() |>
    dplyr::filter(!is.na(line_numbers)) |>
    validate_select(line_numbers, relations_to_hh_head)
}
