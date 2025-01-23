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

sync_logs_from_src_to_des <- function(input_data, uid, db_src, db_des) {

  conn_src <- DBI::dbConnect(RSQLite::SQLite(), db_src)
  conn_des <- DBI::dbConnect(RSQLite::SQLite(), db_des)

  log_tables_src <- DBI::dbListTables(conn_src)
  log_tables <- DBI::dbListTables(conn_des)

  if(!('logs' %in% log_tables_src)) {
    create_db_tables(conn_src, input_data, uid)
  }

  if(!('logs' %in% log_tables)) {
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

  # logs table ----------------------
  logs_src <- dplyr::tbl(conn_src, 'logs') |>
    dplyr::filter(status > 0L)

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
        cv = 0,
        ts = 0,
        remarks = 0
      )
    )
  }

  logs_des <- dplyr::tbl(conn_des, 'logs')

  logs_to_copy <- logs_src |>
    dplyr::collect() |>
    dplyr::mutate(
      created_at = created_at |>
        lubridate::as_datetime() |>
        as.character(),
      updated_at = updated_at |>
        lubridate::as_datetime() |>
        as.character(),
      verified_at = dplyr::if_else(
        grepl('[a-zA-Z]', verified_at),
        verified_at |>
          stringr::str_remove('.*[a-zA-Z]{3}') |>
          stringr::str_remove('GTM.*$') |>
          stringr::str_trim() |>
          lubridate::mdy_hms() |>
          as.character(),
        verified_at |>
          lubridate::as_datetime() |>
          as.character()
      ),
      validated_at = validated_at |>
        lubridate::as_datetime() |>
        as.character()
    ) |>
    dplyr::anti_join(
      logs_des |>
        dplyr::collect() |>
        dplyr::mutate(
          created_at = created_at |>
            lubridate::as_datetime() |>
            as.character(),
          updated_at = updated_at |>
            lubridate::as_datetime() |>
            as.character(),
          verified_at = dplyr::if_else(
            grepl('[a-zA-Z]', verified_at),
            verified_at |>
              stringr::str_remove('.*[a-zA-Z]{3}') |>
              stringr::str_remove('GTM.*$') |>
              stringr::str_trim() |>
              lubridate::mdy_hms() |>
              as.character(),
            verified_at |>
              lubridate::as_datetime() |>
              as.character()
          ),
          validated_at = validated_at |>
            lubridate::as_datetime() |>
            as.character()
        ),
      by = 'id',
      copy = TRUE
    ) |>
    suppressWarnings()

  logs_n <- nrow(logs_to_copy)


  if(logs_n == 0) {

    DBI::dbDisconnect(conn_src, force = T)
    DBI::dbDisconnect(conn_des, force = T)

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

  max_id <- logs_des |>
    dplyr::summarise(max = max(id_int, na.rm = T)) |>
    dplyr::collect() |>
    dplyr::pull(max)

  if(is.na(max_id)) {
    max_id <- 0
  }

  DBI::dbAppendTable(
    conn_des,
    name = 'logs',
    value = logs_to_copy |>
      dplyr::mutate(id_int = dplyr::row_number() + max_id)
  )

  # cv table ----------------------
  cv_table <- glue::glue('{input_data}_cv')
  cv_src <- dplyr::tbl(conn_src, cv_table)
  cv_des <- dplyr::tbl(conn_des, cv_table)

  log_ids <- dplyr::pull(logs_to_copy, id)

  cv_src <- cv_src |>
    dplyr::filter(log_id %in% log_ids)

  cv_to_copy <- cv_src |>
    dplyr::collect() |>
    dplyr::mutate(
      tag_status = tag_status |>
        lubridate::as_datetime() |>
        as.character(),
      updated_at = updated_at |>
        lubridate::as_datetime() |>
        as.character()
    ) |>
    dplyr::anti_join(
      cv_des |>
        dplyr::collect() |>
        dplyr::mutate(
          tag_status = tag_status |>
            lubridate::as_datetime() |>
            as.character(),
          updated_at = updated_at |>
            lubridate::as_datetime() |>
            as.character()
        ),
      by = 'id',
      copy = TRUE
    ) |>
    suppressWarnings()

  cv_n <- nrow(cv_to_copy)

  if(cv_n > 0) {
    DBI::dbAppendTable(
      conn_des,
      name = cv_table,
      value = cv_to_copy
    )
  }

  # ts table --------------------------
  ts_src <- dplyr::tbl(conn_src, 'ts')
  ts_des <- dplyr::tbl(conn_des, 'ts')

  ts_src <- ts_src |>
    dplyr::filter(log_id %in% log_ids)

  ts_to_copy <- ts_src |>
    dplyr::collect() |>
    dplyr::mutate(
      tag_status = tag_status |>
        lubridate::as_datetime() |>
        as.character(),
      updated_at = updated_at |>
        lubridate::as_datetime() |>
        as.character()
    ) |>
    dplyr::anti_join(
      ts_des |>
        dplyr::collect() |>
        dplyr::mutate(
          tag_status = tag_status |>
            lubridate::as_datetime() |>
            as.character(),
          updated_at = updated_at |>
            lubridate::as_datetime() |>
            as.character()
        ),
      by = 'id',
      copy = TRUE
    )

  ts_n <- nrow(ts_to_copy)

  if(ts_n > 0) {
    DBI::dbAppendTable(
      conn_des,
      name = 'ts',
      value = ts_to_copy
    )
  }

  # remarks table ----------------------
  remarks_src <- dplyr::tbl(conn_src, 'remarks')
  remarks_des <- dplyr::tbl(conn_des, 'remarks')

  remarks_to_copy <- remarks_src |>
    dplyr::collect() |>
    dplyr::mutate(
      tag_status = tag_status |>
        lubridate::as_datetime() |>
        as.character(),
      updated_at = updated_at |>
        lubridate::as_datetime() |>
        as.character(),
      created_at = created_at |>
        lubridate::as_datetime() |>
        as.character()
    ) |>
    dplyr::anti_join(
      remarks_des |>
        dplyr::collect() |>
        dplyr::mutate(
          tag_status = tag_status |>
            lubridate::as_datetime() |>
            as.character(),
          updated_at = updated_at |>
            lubridate::as_datetime() |>
            as.character(),
          created_at = created_at |>
            lubridate::as_datetime() |>
            as.character()
        ),
      by = c(
        'uuid',
        'user_id',
        'remarks',
        'status',
        'role',
        'tag_status',
        'created_at',
        'bulk_id'
      ),
      copy = TRUE
    )

  remarks_n <- nrow(remarks_to_copy)

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
      status_code = 1,
      logs = logs_n,
      cv = cv_n,
      ts = ts_n,
      remarks = remarks_n
    )
  )
}

