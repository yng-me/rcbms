#' Title
#'
#' @param path
#'
#' @return
#' @export
#'
#' @examples
#'

create_rcbms_db <- function(path) {

  input_data_list <- c('hp', 'bp', 'ilq')

  for(i in seq_along(input_data_list)) {

    input_data <- input_data_list[i]
    db <- file.path(path, glue::glue("{input_data}_rcbms_logs_v3.db"))
    conn <- DBI::dbConnect(RSQLite::SQLite(), db)
    uid <- 'case_id'
    if(input_data == 'bp') { uid <- 'barangay_geo' }

    rcbms::create_db_tables(conn, input_data, uid)

    res <- DBI::dbExecute(
      conn,
      "ALTER TABLE remarks ADD COLUMN bulk_id integer DEFAULT 0;"
    )

    DBI::dbDisconnect(conn, force = T)

  }
}
