#' Title
#'
#' @param value_current
#' @param value_previous
#' @param ref_current
#' @param ref_previous
#'
#' @return
#' @export
#'
#' @examples

compute_pgr <- function(
    value_current,
    value_previous,
    ref_current  = '2024-07-01',
    ref_previous = '2020-05-01'
) {

  ratio <- value_current / value_previous
  diff <- as.POSIXlt(ref_current) - as.POSIXlt(ref_previous)

  (ratio ^ (1 / ( as.integer(diff) / 365 )) - 1) * 100

}
