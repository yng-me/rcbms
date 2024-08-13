connect_to_rcbms_logs_db <- function(.config, .input_data) {
  wd <- create_new_folder(file.path(.config$base, "data", "log"))
  v <- config$version$db
  if (is.null(v)) v <- "0.0.1"
  db_name <- file.path(wd, paste0(.input_data, "_rcbms_logs_v", v, ".db"))
  conn <- DBI::dbConnect(RSQLite::SQLite(), db_name)
  return(conn)
}
