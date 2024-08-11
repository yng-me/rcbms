#' Title
#'
#' @param .data
#' @param ...
#' @param .input_data
#' @param .references
#' @param .config
#'
#' @return
#' @export
#'
#' @examples
bind_parquet <- function(
    .data,
    ...,
    .input_data = NULL,
    .references = get_config("references"),
    .config = getOption("rcbms.config")) {

  retain_cols <- c()
  final_status <- .config$project[[.input_data]]$final_status$variable


  if (is.null(.input_data)) {
    if (exists("CURRENT_INPUT_DATA")) {
      .input_data <- CURRENT_INPUT_DATA
    } else {
      .input_data <- "hp"
    }
  }

  if (!inherits(.data, "list")) {
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
            final_status,
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
          final_status
        )),
        ...
      ) |>
      dplyr::collect()
  })

  bind_rows(df) |>
    add_metadata(
      .dictionary = .references$data_dictionary,
      .valueset = .references$valueset
    )
}
