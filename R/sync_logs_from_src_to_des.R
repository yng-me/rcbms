#' Title
#'
#' @param input_data
#' @param db_src
#' @param db_des
#'
#' @return
#' @export
#'
#' @examples
#'

sync_logs_from_src_to_des <- function(input_data, db_src, db_des) {

  conn_src <- DBI::dbConnect(RSQLite::SQLite(), db_src)
  conn_des <- DBI::dbConnect(RSQLite::SQLite(), db_des)

  # logs table ----------------------
  logs_src <- dplyr::tbl(conn_src, 'logs')
  logs_des <- dplyr::tbl(conn_des, 'logs')

  logs_to_copy <- logs_src |>
    dplyr::anti_join(logs_des, by = 'id', copy = TRUE)

  logs_n <- logs_to_copy |>
    dplyr::count() |>
    dplyr::pull(n)

  if(logs_n > 0) {
    max_id <- logs_des |>
      dplyr::summarise(max = max(id_int, na.rm = T)) |>
      dplyr::pull(max)

    DBI::dbAppendTable(
      conn_des,
      name = 'logs',
      value = logs_to_copy |>
        dplyr::mutate(id_int = dplyr::row_number() + max_id) |>
        dplyr::collect()
    )
  }

  # cv table ----------------------
  cv_table <- glue::glue('{input_data}_cv')
  cv_src <- dplyr::tbl(conn_src, cv_table)
  cv_des <- dplyr::tbl(conn_des, cv_table)
  log_ids <- c()

  if(logs_n > 0) {
    log_ids <- dplyr::pull(logs_to_copy, id)
    if(length(log_ids) > 0) {
      cv_src <- cv_src |>
        dplyr::filter(log_id %in% log_ids)
    }
  }

  cv_to_copy <- cv_src |>
    dplyr::anti_join(cv_des, by = 'id', copy = TRUE)

  cv_n <- cv_to_copy |>
    dplyr::count() |>
    dplyr::pull(n)

  if(cv_n > 0) {
    DBI::dbAppendTable(
      conn_des,
      name = cv_table,
      value = dplyr::collect(cv_to_copy)
    )
  }


  # ts table --------------------------
  ts_src <- dplyr::tbl(conn_src, 'ts')
  ts_des <- dplyr::tbl(conn_des, 'ts')

  if(logs_n > 0 & length(log_ids) > 0) {
    ts_src <- ts_src |>
      dplyr::filter(log_id %in% log_ids)
  }

  ts_to_copy <- ts_src |>
    dplyr::anti_join(ts_des, by = 'id', copy = TRUE)

  ts_n <- ts_to_copy |>
    dplyr::count() |>
    dplyr::pull(n)

  if(ts_n > 0) {
    DBI::dbAppendTable(
      conn_des,
      name = 'ts',
      value = dplyr::collect(ts_to_copy)
    )
  }

  # remarks table ----------------------
  remarks_src <- dplyr::tbl(conn_src, 'remarks')
  remarks_des <- dplyr::tbl(conn_des, 'remarks')

  remarks_to_copy <- remarks_src |>
    dplyr::anti_join(
      remarks_des,
      by = c(
        'uuid',
        'user_id',
        'remarks',
        'status',
        'created_at',
        'updated_at',
        'bulk_id'
      ),
      copy = TRUE
    )

  remarks_n <- remarks_to_copy |>
    dplyr::count() |>
    dplyr::pull(n)

  if(remarks_n > 0) {

    DBI::dbAppendTable(
      conn_des,
      name = 'remarks',
      value = remarks_to_copy |>
        dplyr::select(-id) |>
        dplyr::collect()
    )
  }

  DBI::dbDisconnect(conn_src, force = T)
  DBI::dbDisconnect(conn_des, force = T)

  return(
    list(
      logs = logs_n,
      cv = cv_n,
      ts = ts_n,
      remarks = remarks_n
    )
  )
}

