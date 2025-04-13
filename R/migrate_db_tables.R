#' Title
#'
#' @param db_des
#' @param db_src
#'
#' @return
#' @export
#'
#' @examples

migrate_rcbms_db <- function(db_des, db_src, db_backup, input_data) {

  conn_des <- DBI::dbConnect(RSQLite::SQLite(), db_des)
  migrate_db_tables(conn_des)

  if(!file.exists(db_src)) return(NULL)

  rcbms::create_new_folder(db_backup)
  file.copy(db_src, db_backup, recursive = T)

  conn_src <- DBI::dbConnect(RSQLite::SQLite(), db_src)

  # Logs -------------------------------------------
  rcbms_logs <- dplyr::tbl(conn_src, 'logs') |>
    dplyr::select(-id_int) |>
    dplyr::rename(log_id = id) |>
    dplyr::filter(status > 0) |>
    dplyr::collect()

  if(nrow(rcbms_logs) == 0) return(NULL)


  DBI::dbBegin(conn_des)

  DBI::dbAppendTable(conn_des, name = 'rcbms_logs', value = rcbms_logs)


  # CV -------------------------------------------
  uid <- 'case_id'
  uid_def <- paste0(rep('0', 31), collapse = '')
  if(input_data == 'bp') {
    uid <- 'barangay_geo'
    uid_def <- paste0(rep('0', 10), collapse = '')
  }

  rcbms_logs_id <- rcbms_logs$log_id

  cv <- dplyr::tbl(conn_src, glue::glue('{input_data}_cv')) |>
    dplyr::filter(log_id %in% rcbms_logs_id) |>
    dplyr::collect()

  if(!('line_number' %in% names(cv))) {
    cv <- dplyr::mutate(cv, line_number = '00')
  }

  if(!('info' %in% names(cv))) {
    cv <- dplyr::mutate(cv, info = NA_character_)
  }

  if(!('summary' %in% names(cv))) {
    cv <- dplyr::mutate(cv, summary = NA_character_)
  }

  if(!('contact' %in% names(cv))) {
    cv <- dplyr::mutate(cv, contact = NA_character_)
  }

  cv <- cv |>
    dplyr::mutate(line_number = dplyr::if_else(is.na(line_number), '00', line_number)) |>
    dplyr::select(
      log_id,
      uuid = id,
      mode_id = validation_id,
      input_data_id = !!as.name(uid),
      line_number,
      info,
      summary,
      contact,
      tag_status,
      updated_at
    )


  # TS -------------------------------------------
  ts <- dplyr::tbl(conn_src, 'ts') |>
    dplyr::filter(log_id %in% rcbms_logs_id) |>
    dplyr::collect()

  if(!(uid %in% names(ts))) {
    ts <- dplyr::mutate(ts, input_data_id = uid_def)
  }

  if(!('line_number' %in% names(ts))) {
    ts <- dplyr::mutate(ts, line_number = '00')
  }

  if(!('info' %in% names(ts))) {
    ts <- dplyr::mutate(ts, info = NA_character_)
  }

  if(!('summary' %in% names(ts))) {
    ts <- dplyr::mutate(ts, summary = NA_character_)
  }

  if(!('contact' %in% names(ts))) {
    ts <- dplyr::mutate(ts, contact = NA_character_)
  }

  ts <- ts |>
    dplyr::select(
      log_id,
      uuid = id,
      mode_id = tabulation_id,
      input_data_id,
      line_number,
      info,
      summary,
      contact,
      tag_status,
      updated_at
    )

  DBI::dbAppendTable(
    conn_des,
    name = 'rcbms_results',
    value = dplyr::select(dplyr::bind_rows(cv, ts), -uuid)
  )

  # Remarks -------------------------------------------
  remarks <- dplyr::tbl(conn_src, 'remarks') |>
    dplyr::filter(status >= 1 & status < 9) |>
    dplyr::select(-id) |>
    dplyr::collect() |>
    dplyr::group_by(uuid) |>
    tidyr::nest(.key = 'remarks_info')


  DBI::dbAppendTable(
    conn_des,
    name = 'rcbms_remarks',
    value = dplyr::bind_rows(cv, ts) |>
      dplyr::left_join(remarks, by = 'uuid') |>
      dplyr::select(
        input_data_id,
        mode_id,
        line_number,
        remarks_info
      ) |>
      tidyr::unnest(remarks_info)
  )

  DBI::dbCommit(conn_des)
  DBI::dbDisconnect(conn_des)

  DBI::dbDisconnect(conn_src)
  unlink(db_src, force = T)

}


migrate_db_tables <- function(.conn) {

  if(DBI::dbExistsTable(.conn, 'rcbms_logs')) return(NULL)

  rcbms_logs <- glue::glue(
    "CREATE TABLE IF NOT EXISTS rcbms_logs (
      id INTEGER PRIMARY KEY,
      log_id VARCHAR(36) NOT NULL UNIQUE,
      survey_round VARCHAR(4),
      mode VARCHAR(16),
      edit TINYINT,
      level TINYINT,
      stage TINYINT,
      category VARCHAR(32),
      station char(8),
      partial TINYINT,
      input_data char(8),
      area_code VARCHAR(32),
      area_codes TEXT,
      number_of_ea_processed INTEGER,
      total_cases INTEGER DEFAULT 0,
      total_cases_unique INTEGER DEFAULT 0,
      total_priority_a INTEGER DEFAULT 0,
      total_priority_b INTEGER DEFAULT 0,
      total_priority_c INTEGER DEFAULT 0,
      total_priority_d INTEGER DEFAULT 0,
      total INTEGER DEFAULT 0,
      summary TEXT,
      user TEXT,
      duration INTEGER,
      source INTEGER DEFAULT 1,
      user_id VARCHAR(36),
      status TINYINT,
      version_app VARCHAR(10),
      version_package VARCHAR(10),
      version_script VARCHAR(10),
      pc_user VARCHAR(32),
      pc_effective_user VARCHAR(32),
      pc_os VARCHAR(16),
      pc_os_release_date VARCHAR(36),
      pc_os_version VARCHAR(36),
      pc_pid INTEGER,
      pc_hardware VARCHAR(16),
      key_app TEXT,
      key_admin TEXT,
      iv_app TEXT,
      iv_admin TEXT,
      created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
      updated_at DATETIME DEFAULT NULL,
      verified_at DATETIME DEFAULT NULL,
      validated_at DATETIME DEFAULT NULL
    );"
  )

  rcbms_remarks <- glue::glue("
    CREATE TABLE IF NOT EXISTS rcbms_remarks (
      id INTEGER PRIMARY KEY,
      bulk_id INTEGER DEFAULT 0,
      input_data_id VARCHAR(36) NOT NULL,
      mode_id VARCHAR(128) NOT NULL,
      line_number VARCHAR(2) NOT NULL DEFAULT '00',
      user_id VARCHAR(36),
      username VARCHAR(36),
      first_name VARCHAR(36),
      last_name VARCHAR(36),
      role VARCHAR(36),
      status TINYINT,
      remarks TEXT,
      uploaded TINYINT,
      created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
      tag_status DATETIME DEFAULT NULL,
      updated_at DATETIME DEFAULT NULL
    );
  ")

  rcbms_results <- glue::glue(
    "CREATE TABLE IF NOT EXISTS rcbms_results (
      id INTEGER PRIMARY KEY,
      log_id VARCHAR(36) NOT NULL,
      mode_id VARCHAR(128) NOT NULL,
      input_data_id VARCHAR(36) NOT NULL,
      line_number VARCHAR(2) NOT NULL DEFAULT '00',
      info TEXT,
      summary TEXT DEFAULT NULL,
      contact TEXT DEFAULT NULL,
      tag_status DATETIME DEFAULT NULL,
      updated_at DATETIME DEFAULT NULL,
      FOREIGN KEY('log_id') REFERENCES logs('log_id')
    );"
  )

  DBI::dbBegin(.conn)
  DBI::dbExecute(.conn, rcbms_logs)
  DBI::dbExecute(.conn, rcbms_remarks)
  DBI::dbExecute(.conn, rcbms_results)

  DBI::dbExecute(.conn, 'CREATE UNIQUE INDEX IF NOT EXISTS log_id_index on rcbms_logs (log_id);')
  DBI::dbExecute(.conn, 'CREATE INDEX IF NOT EXISTS rcbms_logs_user_id_index on rcbms_logs (user_id);')
  DBI::dbExecute(.conn, 'CREATE INDEX IF NOT EXISTS rcbms_logs_type_index on rcbms_logs (input_data, mode, user_id);')
  DBI::dbExecute(.conn, 'CREATE INDEX IF NOT EXISTS rcbms_logs_type_status_index on rcbms_logs (input_data, mode, user_id, status);')

  DBI::dbExecute(.conn, 'CREATE INDEX IF NOT EXISTS rcbms_results_log_id_index on rcbms_results (log_id);')
  DBI::dbExecute(.conn, 'CREATE INDEX IF NOT EXISTS rcbms_results_validation_id_index on rcbms_results (mode_id);')
  DBI::dbExecute(.conn, 'CREATE INDEX IF NOT EXISTS rcbms_results_mode_id_index on rcbms_results (input_data_id);')
  DBI::dbExecute(.conn, 'CREATE INDEX IF NOT EXISTS rcbms_results_pk_index on rcbms_results (input_data_id, mode_id, line_number);')

  DBI::dbExecute(.conn, 'CREATE INDEX IF NOT EXISTS rcbms_remarks_pk_index on rcbms_remarks (input_data_id, mode_id, line_number);')
  DBI::dbExecute(.conn, 'CREATE INDEX IF NOT EXISTS rcbms_remarks_bulk_id_index on rcbms_remarks (bulk_id);')
  DBI::dbExecute(.conn, 'CREATE INDEX IF NOT EXISTS rcbms_remarks_user_id_index on rcbms_remarks (user_id);')
  DBI::dbExecute(.conn, 'CREATE INDEX IF NOT EXISTS rcbms_remarks_status_index on rcbms_remarks (status);')
  DBI::dbCommit(.conn)

}








