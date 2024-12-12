#' Title
#'
#' @param .dir
#' @param .user_id
#' @param .dir_to
#' @param .delete_source
#'
#' @return
#' @export
#'
#' @examples
#'

import_rcbms_logs <- function(.dir, .user_id, .dir_to = 'db-temp', .delete_source = T) {

  exdir <- file.path(.dir, .dir_to)

  db_logs <- list.files(
    exdir,
    recursive = T,
    full.names = T,
    pattern = '(hp|bp|ilq)_rcbms_logs_v.*db$'
  )

  db_dir <- file.path(.dir, 'db', .user_id)
  res <- list()

  for(i in seq_along(db_logs)) {

    db_log_i <- db_logs[i]
    conn_i <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_log_i)

    if('logs' %in% DBI::dbListTables(conn_i)) {

      db_name <- basename(db_log_i) |>
        fs::path_ext_remove() |>
        stringr::str_extract('^(hp|bp|ilq)')

      res[[db_name[[1]]]] <- extract_rcbms_log(conn_i, db_dir)

    }

    DBI::dbDisconnect(conn_i, force = T)

  }

  if(.delete_source) {
    unlink(exdir, recursive = T, force = T)
  }

  return(res)

}



#' Title
#'
#' @param .conn
#' @param .dir
#'
#' @return
#' @export
#'
#' @examples
#'

extract_rcbms_log <- function(.conn, .dir) {

  logs_from <- DBI::dbReadTable(.conn, 'logs') |>
    dplyr::filter(status > 0)

  if(nrow(logs_from) == 0) return(
    list(
      status_code = 0,
      logs = 0,
      cv = 0,
      ts = 0,
      remarks = 0
    )
  )

  db_i <- basename(DBI::dbGetInfo(.conn)$dbname)

  input_data <- stringr::str_extract(db_i, '^(hp|bp|ilq)')

  db_dir <- rcbms::create_new_folder(.dir)

  conn <- DBI::dbConnect(RSQLite::SQLite(), file.path(db_dir, db_i))

  uid_cols <- get_uid_cols(input_data)
  create_db_tables(conn, input_data, uid_cols$uid)

  # logs ---------------
  logs_to <- DBI::dbReadTable(conn, 'logs') |>
    dplyr::select(id)

  logs_diff <- logs_from |>
    dplyr::filter(!(id %in% logs_to$id))

  if(nrow(logs_diff) == 0) {

    DBI::dbDisconnect(conn, force = T)

    return(
      list(
        status_code = 2,
        logs = 0,
        cv = 0,
        ts = 0,
        remarks = 0
      )
    )
  }

  DBI::dbWriteTable(
    conn,
    value = logs_diff |>
      dplyr::mutate(
        id_int = nrow(logs_to) + (1:dplyr::n()),
        source = 2L
      ),
    name = 'logs',
    append = T
  )

  # validation ---------------
  input_cv <- paste0(input_data, '_cv')

  cv_from <- DBI::dbReadTable(.conn, input_cv) |>
    dplyr::filter(as.integer(status) > 0) |>
    dplyr::filter(log_id %in% logs_diff$id)


  # tabulation ---------------
  ts_from <- DBI::dbReadTable(.conn, 'ts') |>
    dplyr::filter(as.integer(status) > 0) |>
    dplyr::filter(log_id %in% logs_diff$id)

  # remarks ---------------
  remarks_diff <- DBI::dbReadTable(.conn, 'remarks') |>
    dplyr::filter(status > 0, status < 9) |>
    dplyr::filter(uuid %in% ts_from$id | uuid %in% cv_from$id)


  out_remarks_from <- remarks_diff
  table_suffix = '_sync_temp'

  if(nrow(cv_from) > 0) {

    remarks_diff <- import_rcbms_log(
      .conn = conn,
      .data = cv_from,
      .remarks = remarks_diff,
      .table_name = input_cv,
      .by_cv_cols = uid_cols$by_cv_cols,
      .uid = uid_cols$uid,
      .table_suffix = table_suffix
    )
  }

  if(nrow(ts_from) > 0) {

    remarks_diff <- import_rcbms_log(
      .conn = conn,
      .data = ts_from,
      .remarks = remarks_diff,
      .table_name = 'ts',
      .by_cv_cols = 'tabulation_id',
      .uid = uid_cols$uid,
      .table_suffix = table_suffix
    )
  }

  # import remaining remarks ---------------
  if(nrow(remarks_diff) > 0) {

    DBI::dbWriteTable(
      conn,
      value = dplyr::select(remarks_diff, -id),
      name = 'remarks',
      append = T
    )
  }

  DBI::dbDisconnect(conn, force = T)

  return(
    list(
      status_code = 1,
      logs = nrow(logs_diff),
      cv = nrow(cv_from),
      ts = nrow(ts_from),
      remarks = nrow(out_remarks_from)
    )
  )
}


get_uid_cols <- function(.input_data, .type = 'validation') {
  uid <- 'case_id'
  type <- paste0(.type, "_id")
  by_cv_cols <- c(uid, type, "line_number")

  if(.input_data == 'shp') {
    uid <- 'cbms_geoid'
    by_cv_cols <- c(uid, type)
  } else if(.input_data == 'bp') {
    uid <- 'barangay_geo'
    by_cv_cols <- c(uid, type)
  }
  return(
    list(
      uid = uid,
      by_cv_cols = by_cv_cols
    )
  )
}

