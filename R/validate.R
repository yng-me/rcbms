#' Title
#'
#' @param .data
#' @param .id
#' @param ...
#' @param .refs
#' @param .title
#' @param .description
#' @param .section
#' @param .priority_level
#' @param .primary_data_item
#'
#' @return
#' @export
#'
#' @examples
#'

validate <- function(
  .data,
  .id,
  ...,
  .refs = NULL,
  .title = get_from_validation_id(.id, .refs, "title"),
  .description = get_from_validation_id(.id, .refs, "description"),
  .section = get_from_validation_id(.id, .refs, "section"),
  .priority_level = get_from_validation_id(.id, .refs, "priority_level"),
  .primary_data_item = get_from_validation_id(.id, .refs, "primary_data_item")
) {

  attr(.data, "validation_id") <- .id
  attr(.data, "title") <- .title
  attr(.data, "description") <- .description
  attr(.data, "section") <- .section
  attr(.data, "priority_level") <- .priority_level
  attr(.data, "primary_data_item") <- .primary_data_item

  return(.data)

}


get_from_validation_id <- function(.validation_id, .refs, .info) {

  if(is.null(.refs)) return(NULL)

  df <- .refs$validation |>
    dplyr::collect() |>
    dplyr::filter(validation_id == .validation_id)

  if(nrow(df) == 0) return(NULL)

  return(df[[.info]][1])
}
