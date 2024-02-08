#' Title
#'
#' @param .data
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
#'
bind_parquet <- function(.data, ...) {
  retain_cols <- c()
  if(!inherits(.data, 'list')) {
    return(
      .data |>
        dplyr::select(
          dplyr::any_of("case_id"),
          dplyr::matches("(_code$|_agg$|_geo$)"),
          dplyr::any_of(c(
            "ean",
            "bsn",
            "husn",
            "hsn",
            "line_number",
            "result_of_visit"
          )),
          ...
        )
    )
  }

  df_list <- names(.data)
  df <- lapply(df_list, \(x) {
    .data[[x]] |>
      dplyr::select(
        dplyr::any_of("case_id"),
        dplyr::matches("(_code$|_agg$|_geo$)"),
        dplyr::any_of(c(
          "ean",
          "bsn",
          "husn",
          "hsn",
          "line_number",
          "result_of_visit"
        )),
        ...
      ) |>
      dplyr::collect()
  })

  bind_rows(df)

}
