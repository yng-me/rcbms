#' Title
#'
#' @param .data
#' @param .id
#' @param ...
#' @param .references
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
  .references = get_config("references"),
  .title = get_from_validation_id(.id, .references, "title"),
  .description = get_from_validation_id(.id, .references, "description"),
  .section = get_from_validation_id(.id, .references, "section"),
  .priority_level = get_from_validation_id(.id, .references, "priority_level"),
  .primary_data_item = get_from_validation_id(.id, .references, "primary_data_item"),
  .validation_type = get_from_validation_id(.id, .references, "validation_type")
) {

  attr(.data, "validation_id") <- .id
  attr(.data, "title") <- .title
  attr(.data, "description") <- .description
  attr(.data, "section") <- .section
  attr(.data, "priority_level") <- .priority_level
  attr(.data, "primary_data_item") <- .primary_data_item

  if(!(.validation_type %in% c(NA, 1:9))) { .validation_type <- 9 }
  attr(.data, "validation_type") <- as.integer(.validation_type)

  set_class(.data, 'rcbms_cv_tbl')

}


get_from_validation_id <- function(.validation_id, .references, .info) {

  if(is.null(.references)) return(NA_character_)

  df <- .references$validation |>
    dplyr::collect() |>
    dplyr::filter(validation_id == .validation_id)

  if(nrow(df) == 0) return(NA_character_)

  return(df[[.info]][1])
}
