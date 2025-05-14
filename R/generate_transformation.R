#' Title
#'
#' @param .tr
#' @param .config
#' @param .refs
#'
#' @return
#' @export
#'
#' @examples
#'
generate_transformation <- function(.tr, .config, .refs) {

  if(exists("CURRENT_INPUT_DATA")) {
    input_data <- CURRENT_INPUT_DATA
  } else {
    input_data <- .config$input_data[1]
  }

  tr_levels <- names(.tr)
  tr_names <- names(.tr[[1]])
  tr_names <- tr_names[tr_names %in% .refs$ref_tr$table_id]

  output <- NULL

  for (i in seq_along(tr_names)) {

    tr_name <- tr_names[i]
    tr_info <- list()

    cli::cli_text('table_id: {tr_name}')

    for(j in seq_along(tr_levels)) {

      tr_level <- tr_levels[j]

      if(.config$verbose) {
        cat(paste0(str_pad(formatC(nrow(tr_df), big.mark = ','), width = 7), ': ', tr_name, '\n'))
      }

      tr_info[[tr_level]] <- .tr[[tr_level]][[tr_name]]

    }

    output_temp <- dplyr::tibble(
      mode_id = tr_name,
      info = as.character(jsonlite::toJSON(tr_info, auto_unbox = T))
    )

    if (i == 1) {
      output <- output_temp |> dplyr::tibble()
    } else {
      output <- output |>
        dplyr::bind_rows(output_temp) |>
        dplyr::tibble()
    }
  }

  input_data_id_length <- 31

  if (input_data == 'bp') {
    input_data_id_length <- 10
  } else if (input_data == 'shp') {
    input_data_id_length <- 21
  }

  save_to_db <- .config$validation$save_to_db
  if(is.null(save_to_db)) save_to_db <- TRUE

  if(save_to_db) {

    log_id <- save_rcbms_log(
      .data = dplyr::mutate(
        output,
        input_data_id = paste0(rep('0', input_data_id_length), collapse = '')
      ),
      .config = .config,
      .metadata = list(
        area_code = .refs$ref_current_area,
        total_cases = 0L,
        total_cases_unique = 0L,
        total_priority_a = 0L,
        total_priority_b = 0L,
        total_priority_c = 0L,
        total_priority_d = 0L,
        partial = 0L,
        status = 0L,
        verified_at = NA_character_,
        validated_at = NA_character_,
        category = NA_character_,
        summary_info = .refs$ref_summary,
        aes = .refs$ref_meta
      )
    )

    if(is.null(output)) output <- list()
    attr(output, 'rcbms_log_id') <- paste0('rcbms_log_id: ', log_id)
  }

  return(output)
}
