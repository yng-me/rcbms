#' Title
#'
#' @param .config
#'
#' @return
#' @export
#'
#' @examples
#'

save_logs <- function(.config = getOption("rcbms.config")) {

  if(rlang::is_false(.config$execute_mode)) return(invisible())

  if(.config$verbose) {
    cli::cli_h1("Saving Logs")
  }

  if(!exists("current_logs_id")) return(invisible())
  current_logs_id <- unique(current_logs_id)

  conn <- connect_to_rcbms_logs(.config)
  log_tables <- RSQLite::dbListTables(conn)

  logs_input_data <- DBI::dbReadTable(conn, "logs") |>
    dplyr::tibble() |>
    dplyr::filter(id %in% current_logs_id) |>
    dplyr::distinct(input_data) |>
    dplyr::pull(input_data)

  for(i in seq_along(logs_input_data)) {

    input_df <- logs_input_data[i]
    cv_name <- paste0(input_df, "_cv")
    cv_name_current <- paste0(input_df, "_cv_current")

    if(input_df %in% c("hp", "ilq")) {
      by_cv_cols <- c("case_id", "validation_id", "line_number")
    } else if(input_df == "bp") {
      by_cv_cols <- c("uuid", "validation_id")
    }

    cv_logs <- DBI::dbReadTable(conn, cv_name) |>
      dplyr::tibble() |>
      dplyr::filter(log_id %in% current_logs_id) |>
      dplyr::mutate(status = 0)

    cv_logs_current <- cv_logs

    if(cv_name_current %in% log_tables) {

      cv_logs_with_remakrs <- DBI::dbReadTable(conn, cv_name_current) |>
        dplyr::tibble() |>
        dplyr::mutate(id_old = id) |>
        dplyr::filter(!is.na(status))

      if(nrow(cv_logs_with_remakrs) > 0) {
        cv_logs_current <- cv_logs |>
          dplyr::left_join(cv_logs_with_remakrs, by = by_cv_cols, multiple = "first") |>
          dplyr::mutate(id = dplyr::if_else(!is.na(id_old), id_old, id)) |>
          dplyr::select(-id_old)
      }
    }

    RSQLite::dbWriteTable(
      conn = conn,
      name = cv_name_current,
      value = cv_logs_current,
      overwrite = TRUE
    )

  }

  DBI::dbDisconnect(conn)

}
