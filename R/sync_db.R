sync_db <- function(src_db, des_db, input_data) {

  conn_src <- DBI::dbConnect(RSQLite::SQLite(), src_db)
  conn_des <- DBI::dbConnect(RSQLite::SQLite(), des_db)

  sync_db_prep(conn_src, conn_des)
  sync_db_logs(conn_src, conn_des)
  sync_db_remarks(conn_src, conn_des)
  sync_db_ts(conn_src, conn_des)
  sync_db_cv(conn_src, conn_des, input_data)
}


sync_db_prep <- function(conn_src, conn_des) {

  if(!('logs' %in% DBI::dbListTables(conn_src))) {
    create_db_tables(conn_src, input_data, uid)
  }

  if(!('logs' %in% DBI::dbListTables(conn_des))) {
    create_db_tables(conn_des, input_data, uid)
  }

  remarks_fields <- DBI::dbListFields(conn_src, 'remarks')

  if(!('bulk_id' %in% remarks_fields)) {
    res <- DBI::dbExecute(
      conn_src,
      "ALTER TABLE remarks ADD COLUMN bulk_id integer DEFAULT 0;"
    )

    res <- DBI::dbExecute(
      conn_src,
      "UPDATE remarks SET bulk_id = -1 WHERE remarks LIKE '%(bulk update)%' OR remarks LIKE '%(buld update)%';"
    )
  }

  remarks_fields <- DBI::dbListFields(conn_des, 'remarks')

  if(!('bulk_id' %in% remarks_fields)) {
    res <- DBI::dbExecute(
      conn_des,
      "ALTER TABLE remarks ADD COLUMN bulk_id integer DEFAULT 0;"
    )

    res <- DBI::dbExecute(
      conn_des,
      "UPDATE remarks SET bulk_id = -1 WHERE remarks LIKE '%(bulk update)%' OR remarks LIKE '%(buld update)%';"
    )
  }
}

sync_db_logs <- function(conn_src, conn_des) {

  cols_logs <- DBI::dbListFields(conn_des, 'logs')
  cols_logs <- cols_logs[cols_logs != 'id_int']
  cols_q <- paste(rep("?", length(cols_logs)), collapse = ", ")
  cols_logs <- paste(cols_logs, collapse = ", ")

  logs_src <- dplyr::tbl(conn_src, 'logs') |>
    dplyr::filter(status > 0L) |>
    dplyr::collect()

  DBI::dbExecute(
    conn_des,
    glue::glue("INSERT OR IGNORE INTO logs ({cols_logs}) VALUES ({cols_q});"),
    params = as.list(logs_src)
  )

}

sync_db_remarks <- function(conn_src, conn_des) {

  cols_remarks <- DBI::dbListFields(conn_des, 'remarks')
  cols_remarks <- cols_remarks[cols_remarks != 'id']
  cols_q <- paste(rep("?", length(cols_remarks)), collapse = ", ")
  cols_remarks <- paste(cols_remarks, collapse = ", ")

  remarks_src <- dplyr::tbl(conn_src, 'remarks') |>
    dplyr::filter(status > 0L) |>
    dplyr::collect()

  DBI::dbExecute(
    conn_des,
    glue::glue("INSERT OR IGNORE INTO remarks ({cols_remarks}) VALUES ({cols_q});"),
    params = as.list(remarks_src)
  )
}

sync_db_ts <- function(conn_src, conn_des) {

  cols_ts <- DBI::dbListFields(conn_des, 'ts')
  cols_q <- paste(rep("?", length(cols_ts)), collapse = ", ")
  cols_ts <- paste(cols_ts, collapse = ", ")

  ts_src <- dplyr::tbl(conn_src, 'ts') |>
    dplyr::filter(status > 0L) |>
    dplyr::collect()

  DBI::dbExecute(
    conn_des,
    glue::glue("INSERT OR IGNORE INTO ts ({cols_ts}) VALUES ({cols_q});"),
    params = as.list(ts_src)
  )
}

sync_db_cv <- function(conn_src, conn_des, input_data) {

  cv_table <- glue::glue('{input_data}_cv')
  cols_cv <- DBI::dbListFields(conn_des, cv_table)
  cols_q <- paste(rep("?", length(cols_cv)), collapse = ", ")
  cols_cv <- paste(cols_cv, collapse = ", ")

  cv_src <- dplyr::tbl(conn_src, cv_table) |>
    dplyr::filter(status > 0L) |>
    dplyr::collect()

  DBI::dbExecute(
    conn_des,
    glue::glue("INSERT OR IGNORE INTO {cv_table} ({cols_cv}) VALUES ({cols_q});"),
    params = as.list(cv_src)
  )
}
