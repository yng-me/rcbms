#' Title
#'
#' @param path
#' @param db_dir
#'
#' @return
#' @export
#'
#' @examples
#'

sync_rcbms_logs <- function(db_dir, path = '//localhost/webdav') {

  rcbms_db_files <- list.files(
    path,
    recursive = T,
    pattern = '(hp|ilq|bp)_rcbms_logs_.*\\.db',
    full.names = T
  )

  if(length(rcbms_db_files) == 0) {
    return(cat('empty'))
  }

  for(i in seq_along(rcbms_db_files)) {

    db_log_i <- rcbms_db_files[i]
    conn_i <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_log_i)

    if('logs' %in% DBI::dbListTables(conn_i)) {

      extract_rcbms_log(conn_i, db_dir)

    }

    DBI::dbDisconnect(conn_i, force = T)

  }

  unlink(rcbms_db_files, recursive = T, force = T)

  cat('success')

}


