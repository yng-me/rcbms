#' Title
#'
#' @param path
#' @param input_data_list
#' @param version
#'
#' @return
#' @export
#'
#' @examples
#'

create_rcbms_db <- function(path, input_data_list = c('hp', 'bp', 'ilq'), version = 3) {

  for(i in seq_along(input_data_list)) {

    create_new_folder(path)

    input_data <- input_data_list[i]
    db <- file.path(path, glue::glue("{input_data}_rcbms_logs_v{version}.db"))
    conn <- DBI::dbConnect(RSQLite::SQLite(), db)
    uid <- 'case_id'
    if(input_data == 'bp') { uid <- 'barangay_geo' }

    rcbms::create_db_tables(conn, input_data, uid)


    if(!('bulk_id' %in% DBI::dbListFields(conn, 'remarks'))) {
      DBI::dbExecute(
        conn,
        "ALTER TABLE remarks ADD COLUMN bulk_id integer DEFAULT 0;"
      )
    }

    DBI::dbDisconnect(conn, force = T)

  }
}
