#' Title
#'
#' @param .ts
#' @param .ts_ref
#' @param .config
#' @param .save_as_excel
#' @param .save_as_json
#' @param .section_ref
#' @param .summary_df
#'
#' @return
#' @export
#'
#' @examples

generate_tabulation <- function(
  .ts,
  .ts_ref,
  .config,
  .save_as_excel = FALSE,
  .save_as_json = TRUE,
  .section_ref = NULL,
  .summary_df = NULL
) {

  if(exists("CURRENT_INPUT_DATA")) {
    input_data <- CURRENT_INPUT_DATA
  } else {
    input_data <- .config$input_data[1]
  }

  ts_names <- names(.ts)
  ts_names <- ts_names[ts_names %in% .ts_ref$tabulation_id]

  output <- NULL

  for (i in seq_along(ts_names)) {

    ts_name <- ts_names[i]

    if(.config$verbose) {
      cat(paste0(str_pad(formatC(nrow(.ts[[ts_name]]), big.mark = ','), width = 7), ': ', ts_name, '\n'))
    }

    output_temp <- dplyr::tibble(
      tabulation_id = ts_name,
      info = as.character(jsonlite::toJSON(.ts[[ts_name]], auto_unbox = T))
    )

    if (i == 1) {
      output <- output_temp |> dplyr::tibble()
    } else {
      output <- output |>
        dplyr::bind_rows(output_temp) |>
        dplyr::tibble()
    }
  }


  save_to_db <- .config$validation$save_to_db
  if(is.null(save_to_db)) save_to_db <- TRUE

  if(save_to_db) {
    log_id <- save_rcbms_logs(output, input_data, .ts_ref, .config, .section_ref, .summary_df)
    if(is.null(output)) output <- list()
    attr(output, 'rcbms_log_id') <- paste0('rcbms_log_id: ', log_id)
  }

  return(output)
}
