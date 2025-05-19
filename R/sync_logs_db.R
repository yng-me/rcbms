#' Title
#'
#' @param db_src
#' @param db_des
#' @param input_data
#'
#' @return
#' @export
#'
#' @examples

sync_logs_db <- function(db_src, db_des, input_data) {

  conn_src <- DBI::dbConnect(RSQLite::SQLite(), db_src)
  conn_des <- DBI::dbConnect(RSQLite::SQLite(), db_des)

  if(!DBI::dbExistsTable(conn_src, 'rcbms_logs') | !DBI::dbExistsTable(conn_des, 'rcbms_logs')) {
    return(
      list(
        status_code = 0,
        logs = 0,
        results = 0,
        remarks = 0
      )
    )
  }

  logs_src <- dplyr::tbl(conn_src, 'rcbms_logs') |>
    dplyr::filter(status > 0L) |>
    dplyr::select(-dplyr::any_of('id'))

  n_logs_src <- logs_src |>
    dplyr::select(1) |>
    dplyr::count() |>
    dplyr::collect()

  if(n_logs_src$n == 0) {
    DBI::dbDisconnect(conn_src, force = T)
    DBI::dbDisconnect(conn_des, force = T)
    return(
      list(
        status_code = 0,
        logs = 0,
        results = 0,
        remarks = 0
      )
    )
  }

  logs_des_ids <- dplyr::tbl(conn_des, 'rcbms_logs') |>
    dplyr::select(log_id) |>
    dplyr::collect() |>
    dplyr::pull()

  logs_src_to_copy <- logs_src |>
    dplyr::filter(!(log_id %in% logs_des_ids)) |>
    dplyr::collect()

  if(nrow(logs_src_to_copy) == 0) {
    DBI::dbDisconnect(conn_src, force = T)
    DBI::dbDisconnect(conn_des, force = T)
    return(
      list(
        status_code = 2,
        logs = 0,
        results = 0,
        remarks = 0
      )
    )
  }

  results_to_copy <- dplyr::tbl(conn_src, 'rcbms_results') |>
    dplyr::filter(!(log_id %in% logs_des_ids)) |>
    dplyr::select(-dplyr::any_of('id')) |>
    dplyr::collect()

  remarks_to_copy <- dplyr::tbl(conn_src, 'rcbms_remarks') |>
    dplyr::select(-dplyr::any_of('id')) |>
    dplyr::filter(status >= 1L & status < 9L) |>
    dplyr::collect() |>
    dplyr::mutate(
      tag_status = as.character(tag_status),
      created_at = as.character(created_at),
      updated_at = as.character(updated_at)
    ) |>
    dplyr::anti_join(
      dplyr::tbl(conn_des, 'rcbms_remarks') |>
        dplyr::select(-dplyr::any_of('id')) |>
        dplyr::filter(status >= 1L & status < 9L) |>
        dplyr::collect() |>
        dplyr::mutate(
          tag_status = as.character(tag_status),
          created_at = as.character(created_at),
          updated_at = as.character(updated_at)
        ),
      by = c(
        'bulk_id',
        'input_data_id',
        'mode_id',
        'line_number',
        'user_id',
        'remarks',
        'status',
        'tag_status',
        'created_at'
      )
    ) |>
    dplyr::distinct(
      bulk_id,
      input_data_id,
      mode_id,
      line_number,
      user_id,
      remarks,
      status,
      created_at,
      .keep_all = T
    )

  private_keys_id <- dplyr::tbl(conn_des, 'rcbms_private_keys') |>
    dplyr::select(key_id) |>
    dplyr::collect() |>
    dplyr::pull()

  private_keys_to_copy <- dplyr::tbl(conn_src, 'rcbms_private_keys') |>
    dplyr::select(-dplyr::any_of('id')) |>
    dplyr::filter(is.na(deleted_at)) |>
    dplyr::filter(!(key_id %in% private_keys_id)) |>
    dplyr::collect()

  DBI::dbDisconnect(conn_src, force = T)

  DBI::dbBegin(conn_des)

  DBI::dbAppendTable(conn_des, name = 'rcbms_logs', value = logs_src_to_copy)

  if(nrow(results_to_copy) > 0) {
    DBI::dbAppendTable(conn_des, name = 'rcbms_results', value = results_to_copy)
  }

  if(nrow(remarks_to_copy) > 0) {
    DBI::dbAppendTable(conn_des, name = 'rcbms_remarks', value = remarks_to_copy)
  }

  if(nrow(private_keys_to_copy) > 0) {
    DBI::dbAppendTable(conn_des, name = 'rcbms_private_keys', value = private_keys_to_copy)
  }

  DBI::dbCommit(conn_des)
  DBI::dbDisconnect(conn_des, force = T)

  return(
    list(
      status_code = 1,
      logs = nrow(logs_src_to_copy),
      results = nrow(results_to_copy),
      remarks = nrow(remarks_to_copy)
    )
  )

}



#' Title
#'
#' @param db_src
#' @param db_des
#' @param input_data
#' @param current_log_id
#'
#' @return
#' @export
#'
#' @examples
#'

sync_current_log <- function(db_src, db_des, input_data, current_log_id, remarks_only = F) {

  conn_src <- DBI::dbConnect(RSQLite::SQLite(), db_src)

  log_to_append <- dplyr::tbl(conn_src, 'rcbms_logs') |>
    dplyr::filter(log_id == current_log_id) |>
    dplyr::select(-dplyr::any_of('id')) |>
    dplyr::collect()

  if(nrow(log_to_append) == 0) {
    DBI::dbDisconnect(conn_src, force = T)
    return(NULL)
  }

  results_to_append <- dplyr::tbl(conn_src, 'rcbms_results') |>
    dplyr::filter(log_id == current_log_id) |>
    dplyr::select(-dplyr::any_of('id')) |>
    dplyr::collect()

  remarks_to_append <- dplyr::tbl(conn_src, 'rcbms_remarks') |>
    dplyr::select(-dplyr::any_of('id')) |>
    dplyr::filter(status >= 1L & status < 9L) |>
    dplyr::filter(is.na(tag_status)) |>
    dplyr::collect() |>
    dplyr::distinct()

  DBI::dbDisconnect(conn_src, force = T)

  conn_des <- DBI::dbConnect(RSQLite::SQLite(), db_des)
  DBI::dbBegin(conn_des)
  DBI::dbAppendTable(conn_des, name = 'rcbms_logs', value = log_to_append)

  if(nrow(results_to_append) > 0) {
    DBI::dbAppendTable(conn_des, name = 'rcbms_results', value = results_to_append)
  }

  if(nrow(remarks_to_append) > 0) {
    DBI::dbAppendTable(conn_des, name = 'rcbms_remarks', value = remarks_to_append)
    DBI::dbExecute(
      conn_des,
      "UPDATE rcbms_remarks
      SET tag_status = CURRENT_TIMESTAMP
      WHERE tag_status IS NULL;"
    )
  }

  DBI::dbCommit(conn_des)
  DBI::dbDisconnect(conn_des, force = T)

}
