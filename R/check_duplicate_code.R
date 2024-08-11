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
check_duplicate_relation <- function(
  .data,
  .response_code,
  .config = getOption("rcbms.config")
) {

  var <- .config$project$hp$variable
  relation_to_hh_head_var <- var$relation_to_hh_head

  if ("a02_relation_to_hh_head" %in% names(.data)) {
    rel <- "a02_relation_to_hh_head"
  } else if (relation_to_hh_head_var %in% names(.data)) {
    rel <- relation_to_hh_head_var
  } else {
    rel <- NA_character_
  }

  .data |>
    check_duplicate_code(
      .var = !!as.name(rel),
      .response_code = .response_code
    )
}



#' Title
#'
#' @param .data
#' @param .var
#' @param ...
#' @param .response_code
#' @param .config
#'
#' @return
#' @export
#'
#' @examples
check_duplicate_code <- function(
  .data,
  .var,
  .response_code,
  ...,
  .included_cols = NULL,
  .config = getOption("rcbms.config")
) {

  var <- .config$project$hp$variable
  line_number_var <- var$line_number
  stringify_info <- .config$validation$stringify_info
  if(is.null(stringify_info)) stringify_info <- TRUE

  .data <- .data |>
    dplyr::filter({{.var}} %in% .response_code) |>
    dplyr::select(
      case_id,
      ...,
      dplyr::any_of(c("line_number", line_number_var, .included_cols)),
      {{.var}}
    ) |>
    dplyr::add_count(case_id, ...) |>
    dplyr::filter(n > 1) |>
    dplyr::select(-n) |>
    dplyr::group_by(case_id) |>
    tidyr::nest()

  if(stringify_info) {
    .data |>
      mutate(data = purrr::map(data, \(x) {
        x |>
          dplyr::mutate_all(as.character) |>
          jsonlite::toJSON() |>
          as.character()
      })) |>
      select_cv(data)
  } else {
    .data |>
      select_cv(data)
  }
}
