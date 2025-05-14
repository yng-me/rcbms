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
check_ethnicity <- function(
    .data,
    .var,
    .relation_var,
    .primary_member,
    .relation_to_primary_member,
    .config = getOption("rcbms.config")
) {

  stringify_info <- .config$validation$stringify_info
  if(is.null(stringify_info)) stringify_info <- TRUE

  .data <- .data |>
    dplyr::filter(
      {{.relation_var}} %in% c(.primary_member, .relation_to_primary_member)
    ) |>
    dplyr::select(
      case_id,
      dplyr::any_of("line_number"),
      {{.var}},
      {{.relation_var}}
    ) |>
    dplyr::group_by(case_id) |>
    tidyr::nest() |>
    dplyr::mutate(
      data = purrr::map(data, \(x) {
        y <- x |>
          dplyr::filter({{.relation_var}} %in% .relation_to_primary_member) |>
          dplyr::pull({{.var}})

        x |> dplyr::filter(!({{.var}} %in% y))
      })
    ) |>
    dplyr::filter(purrr::map_int(data, nrow) > 0)


  if(stringify_info) {
    .data |>
      dplyr::mutate(data = purrr::map(data, \(x) {
        x |> dplyr::mutate_all(as.character)
      })) |>
      dplyr::mutate(data = as.character(jsonlite::toJSON(data))) |>
      select_cv(data)
  } else {
    .data |>
      select_cv(data)
  }
}
