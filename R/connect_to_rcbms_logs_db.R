connect_to_rcbms_logs_db <- function(.config, .input_data) {
  wd <- create_new_folder(file.path(.config$base, "data", "log"))
  v <- config$version$db_next
  if (is.null(v)) v <- "4"
  db_name <- file.path(wd, paste0(.input_data, "_rcbms_logs_v", v, ".db"))
  conn <- DBI::dbConnect(RSQLite::SQLite(), db_name)
  return(conn)
}


connect_to_db_log <- function(.config, .input_data) {

  if(is.null(.config$user_id)) {
    conn <- connect_to_rcbms_logs_db(.config, .input_data)
  } else {
    wd <- create_new_folder(file.path("..", "logs", "db", .config$user_id))
    v <- config$version$db_next
    if (is.null(v)) v <- "4"
    db_name <- file.path(wd, paste0(.input_data, "_rcbms_logs_v", v, ".db"))
    conn <- DBI::dbConnect(RSQLite::SQLite(), db_name)
  }

  return(conn)
}
