#' Title
#'
#' @param .ts
#' @param .ts_ref
#' @param .config
#' @param .save_as_excel
#' @param .save_as_json
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
  .save_as_json = TRUE
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

    output_temp <- .ts[[ts_name]] |>
      tidyr::nest(.key = "info") |>
      dplyr::mutate(
        tabulation_id = ts_name,
        info = purrr::map_chr(info, \(x) {
          x |>
            dplyr::mutate_all(as.character) |>
            jsonlite::toJSON() |>
            as.character()
        })
      ) |>
      dplyr::ungroup()

    if (i == 1) {
      output <- output_temp |> dplyr::tibble()
    } else {
      output <- output |>
        dplyr::bind_rows(output_temp) |>
        dplyr::tibble()
    }
  }

  save_rcbms_logs(output, input_data, .ts_ref, .config)

  return(output)
}
