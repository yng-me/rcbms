factor_cols <- function(.data, ..., .keep_cols = TRUE) {
  cols <-  sapply(substitute(list(...))[-1], deparse)
  for(i in seq_along(cols)) {
    .data <- .data |>
      factor_col(
        as.character(cols[[i]]),
        .keep_cols = .keep_cols
      )
  }
  .data
}


factor_col <- function(.data, .col, .keep_cols = TRUE) {

  attr_i <- attributes(.data[[.col]])

  if(!is.null(attr_i$valueset)) {

    col_fct <- .col
    if(.keep_cols) col_fct <- paste0(.col, "_fct")
    .data <- .data |>
      dplyr::mutate(
        !!as.name(col_fct) := factor(
          as.integer(!!as.name(.col)),
          as.integer(attr_i$valueset$value),
          attr_i$valueset$label
        )
      )
    attr(.data[[col_fct]], "label") <- attr_i$label
  }
  .data
}
