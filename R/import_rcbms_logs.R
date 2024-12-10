#' Title
#'
#' @param .dir
#' @param .user_id
#'
#' @return
#' @export
#'
#' @examples

import_rcbms_logs <- function(.dir, .user_id) {

  exdir <- file.path(.dir, 'db-temp')

  db_logs <- list.files(
    exdir,
    recursive = T,
    full.names = T,
    pattern = '(hp|bp|ilq)_rcbms_logs_v.*db$'
  )

  done <- NULL
  status <- 'empty'

  db_dir <- file.path(.dir, 'db', .user_id)

  for(i in seq_along(db_logs)) {

    db_log_i <- db_logs[i]
    conn_i <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_log_i)

    if('logs' %in% DBI::dbListTables(conn_i)) {

      extract_rcbms_log(conn_i, db_dir)

    }

    DBI::dbDisconnect(conn_i, force = T)

  }

  # unlink(exdir, recursive = T, force = T)
  # print('success')

}




extract_rcbms_log <- function(.conn, .dir) {

  logs_from <- DBI::dbReadTable(.conn, 'logs') |>
    dplyr::filter(status > 0)

  if(nrow(logs_from) == 0) return(NULL)

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
    return(NULL)
  }

  DBI::dbWriteTable(
    conn,
    value = logs_diff |>
      dplyr::mutate(id_int = nrow(logs_to) + (1:dplyr::n())),
    name = 'logs',
    append = T
  )

  # tabulation ---------------
  ts_from <- DBI::dbReadTable(.conn, 'ts') |>
    dplyr::filter(as.integer(status) > 0) |>
    dplyr::filter(log_id %in% logs_diff$id)

  if(nrow(ts_from) > 0) {

    remarks_i <- import_rcbms_log(
      .conn = conn,
      .data = ts_from,
      .remarks = remarks_i,
      .input_data = input_data,
      .name = 'ts'
    )

  }


  # validation ---------------
  input_cv <- paste0(input_data, '_cv')

  cv_from <- DBI::dbReadTable(.conn, input_cv) |>
    dplyr::filter(as.integer(status) > 0) |>
    dplyr::filter(log_id %in% logs_diff$id)

  if(nrow(cv_from) > 0) {

    # remarks_i <- import_rcbms_log(
    #   .conn = conn,
    #   .data = cv_i,
    #   .remarks = remarks_i,
    #   .input_data = input_data,
    #   .name = input_cv
    # )
  }

  # remarks ---------------
  remarks_from <- DBI::dbReadTable(.conn, 'remarks') |>
    dplyr::filter(status > 0) |>
    dplyr::filter(uuid %in% ts_from$id | uuid %in% cv_from$id)

  # import remaining remarks ---------------
  if(nrow(remarks_i) > 0) {

    DBI::dbWriteTable(
      conn,
      value = remarks_i |> dplyr::select(-id),
      name = 'remarks',
      append = T
    )
  }

  DBI::dbDisconnect(conn, force = T)

}


get_uid_cols <- function(.input_data) {
  uid <- 'case_id'
  by_cv_cols <- c(uid, "validation_id", "line_number")

  if(.input_data == 'shp') {
    uid <- 'cbms_geoid'
    by_cv_cols <- c(uid, "validation_id")
  } else if(.input_data == 'bp') {
    uid <- 'barangay_geo'
    by_cv_cols <- c(uid, "validation_id")
  }
  return(
    list(
      uid = uid,
      by_cv_cols = by_cv_cols
    )
  )
}

