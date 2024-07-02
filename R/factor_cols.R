factor_cols <- function(.data, ..., .complete = TRUE, .keep_cols = TRUE) {

  cols <- sapply(substitute(list(...))[-1], deparse)

  if(.complete) {

    if('total_y' %in% names(.data)) {

      .data <- .data |>
        dplyr::ungroup() |>
        tidyr::complete(
          !!as.name(cols[1]),
          tidyr::nesting(!!as.name(cols[2]), area_code, total, total_y)
        ) |>
        dplyr::select(
          area_code,
          dplyr::starts_with('total'),
          ...,
          dplyr::everything()
        )

      # print(.data)

    } else {

      .data <- .data |>
        dplyr::ungroup() |>
        tidyr::complete(
          ...,
          tidyr::nesting(area_code, total)
        ) |>
        dplyr::select(
          area_code,
          dplyr::starts_with('total'),
          ...,
          dplyr::everything()
        )
    }

  }

  for (i in seq_along(cols)) {
    .data <- .data |>
      factor_col(
        as.character(cols[[i]]),
        .keep_cols = .keep_cols
      )
  }


  .data |>
    dplyr::ungroup() |>
    dplyr::mutate_if(is.numeric, ~ dplyr::if_else(is.na(.), 0, .))
}


factor_col <- function(.data, .col, .keep_cols = TRUE) {
  attr_i <- attributes(.data[[.col]])

  if (!is.null(attr_i$valueset)) {
    col_fct <- .col
    if (.keep_cols) col_fct <- paste0(.col, "_fct")

    if(is.numeric(as.integer(attr_i$valueset$value[1]))) {

      .data <- .data |>
        dplyr::mutate(
          !!as.name(col_fct) := factor(
            as.integer(!!as.name(.col)),
            as.integer(attr_i$valueset$value),
            attr_i$valueset$label
          )
        )

    } else {
      .data <- .data |>
        dplyr::mutate(
          !!as.name(col_fct) := factor(
            !!as.name(.col),
            attr_i$valueset$value,
            attr_i$valueset$label
          )
        )
    }

    attr(.data[[col_fct]], "label") <- attr_i$label
  }
  .data
}
