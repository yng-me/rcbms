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

  if('a02_relation_to_hh_head' %in% names(.data)) {
    rel <- 'a02_relation_to_hh_head'
  } else if(relation_to_hh_head_var %in% names(.data)) {
    rel <- relation_to_hh_head_var
  } else {
    rel <- NA_character_
  }

  .data |>
    check_duplicate_code(.var = !!as.name(rel), .response_code = .relation) |>
    dplyr::rename(relations_to_hh_head = check_vars)
}



#' Title
#'
#' @param .data
#' @param .var
#' @param .response_cod
#' @param .config
#'
#' @return
#' @export
#'
#' @examples
#'
check_duplicate_code <- function(
  .data,
  .var,
  .response_code,
  .config = getOption("rcbms.config")
) {

  var <- .config$project$hp$variable
  line_number_var <- var$line_number
  g <- dplyr::group_vars(.data)

  .data |>
    dplyr::select(
      case_id,
      dplyr::matches('(_geo|_agg|_code)$'),
      dplyr::any_of(c('line_number', line_number_var, g)),
      var = {{.var}}
    ) |>
    dplyr::group_by(case_id, dplyr::pick(dplyr::matches('(_geo|_agg|_code)$')), .add = T) |>
    tidyr::nest() |>
    dplyr::mutate(
      data = purrr::map(data, \(x) {
        matched <- x |>
          dplyr::filter(var == .response_code)

        if(nrow(matched) > 1) {

          if('line_number' %in% names(matched)) {
            lno <- paste(stringr::str_pad(matched$line_number, width = 2, pad = '0'), collapse = ', ')
          } else if(line_number_var %in% names(matched)) {
            lno <- paste(stringr::str_pad(matched[line_number_var], width = 2, pad = '0'), collapse = ', ')
          } else {
            lno <- NA_character_
          }

          check_var_c <- paste(stringr::str_pad(matched$var, width = 2, pad = '0'), collapse = ', ')

          x |>
            utils::head(1) |>
            dplyr::mutate(line_numbers = lno, check_vars = check_var_c)

        } else {
          x |>
            dplyr::mutate(line_numbers = NA_character_, check_vars = NA_character_) |>
            dplyr::filter_all(dplyr::all_vars(is.na(.)))
        }
      })
    ) |>
    tidyr::unnest(data) |>
    dplyr::ungroup() |>
    dplyr::filter(!is.na(check_vars)) |>
    select_cv(line_numbers, check_vars, dplyr::any_of(g))
}

