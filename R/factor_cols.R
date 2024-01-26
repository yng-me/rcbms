#' Title
#'
#' @param .df
#' @param .col
#'
#' @return
#' @export
#'
#' @examples
#'
factor_cols <- function(.df, .col) {

  col <- stringr::str_remove(rlang::expr_text(rlang::enquo(.col)), '~')

  attr_i <- attributes(.df[[col]])

  if(!is.null(attr_i$valueset)) {
    .df <- .df |>
      dplyr::mutate(
        !!as.name(col) := factor(
          as.integer(!!as.name(col)),
          as.integer(attr_i$valueset$value),
          attr_i$valueset$label
        )
      )
    attr(.df[[col]], "label") <- attr_i$label
  }
  .df
}
