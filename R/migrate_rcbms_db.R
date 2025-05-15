#' Title
#'
#' @param input_data
#' @param db_des
#' @param db_src
#' @param db_backup
#' @param wal_mode
#'
#' @return
#' @export
#'
#' @examples

migrate_rcbms_db <- function(input_data, db_des, db_src = NULL, db_backup = NULL, wal_mode = FALSE) {

  migration_status <- migrate_db_tables(db_des, wal_mode = wal_mode)

  if(is.null(db_src)) return(NULL)
  if(!file.exists(db_src)) return(NULL)

  if(!is.null(db_backup)) {
    rcbms::create_new_folder(db_backup)
    file.copy(db_src, db_backup, recursive = T)
  }

  conn_src <- DBI::dbConnect(RSQLite::SQLite(), db_src)

  if(!DBI::dbExistsTable(conn_src, 'logs')) {
    DBI::dbDisconnect(conn_src, force = T)
    unlink(db_src, force = T, recursive = T)
    return(NULL)
  }

  # Logs -------------------------------------------
  rcbms_logs <- dplyr::tbl(conn_src, 'logs') |>
    dplyr::select(-id_int) |>
    dplyr::rename(log_id = id) |>
    dplyr::filter(status > 0) |>
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
    suppressWarnings()

  if(nrow(rcbms_logs) == 0) {
    DBI::dbDisconnect(conn_src, force = T)
    unlink(db_src, force = T, recursive = T)
    return(NULL)
  }

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

  # Remarks -------------------------------------------
  remarks <- dplyr::tbl(conn_src, 'remarks') |>
    dplyr::filter(status >= 1 & status < 9) |>
    dplyr::select(-dplyr::any_of(c('id', 'uploaded'))) |>
    dplyr::collect()

  if(nrow(remarks) > 0) {

    if(!('bulk_id' %in% names(remarks))) {
      remarks <- dplyr::mutate(remarks, bulk_id = '__default__', .before = 2)
    } else {
      remarks <- remarks |>
        dplyr::group_by(bulk_id) |>
        tidyr::nest() |>
        dplyr::ungroup() |>
        dplyr::mutate(
          bulk_id = dplyr::if_else(
            bulk_id == 0,
            '__default__',
            purrr::map_chr(bulk_id, function(x) uuid::UUIDgenerate()),
            '__default__'
          )
        ) |>
        tidyr::unnest(data)
    }

    remarks <- remarks |>
      dplyr::distinct(.keep_all = T) |>
      dplyr::group_by(uuid) |>
      tidyr::nest(.key = 'remarks_info')

  }


  DBI::dbDisconnect(conn_src, force = T)

  conn_des <- DBI::dbConnect(RSQLite::SQLite(), db_des)
  DBI::dbBegin(conn_des)
  DBI::dbAppendTable(conn_des, name = 'rcbms_logs', value = rcbms_logs)

  results_all <- dplyr::bind_rows(cv, ts)

  if(nrow(results_all) > 0) {

    DBI::dbAppendTable(
      conn_des,
      name = 'rcbms_results',
      value = dplyr::select(results_all, -uuid)
    )

  }

  if(nrow(remarks) > 0 & nrow(results_all) > 0) {
    DBI::dbAppendTable(
      conn_des,
      name = 'rcbms_remarks',
      value = results_all |>
        dplyr::left_join(remarks, by = 'uuid') |>
        dplyr::select(
          input_data_id,
          mode_id,
          line_number,
          remarks_info
        ) |>
        tidyr::unnest(remarks_info) |>
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
    )
  }


  DBI::dbCommit(conn_des)
  DBI::dbDisconnect(conn_des, force = T)

  if(file.exists(db_src)) {
    unlink(db_src, force = T, recursive = T)
  }

}


migrate_db_tables <- function(db, wal_mode = FALSE) {

  conn <- DBI::dbConnect(RSQLite::SQLite(), db)

  if(DBI::dbExistsTable(conn, 'rcbms_logs')) {
    DBI::dbDisconnect(conn, force = T)
    return(0)
  }

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
      bulk_id VARCHAR(36) DEFAULT '__default__',
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
      created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
      updated_at DATETIME DEFAULT NULL,
      tag_status DATETIME DEFAULT NULL
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
      FOREIGN KEY('log_id') REFERENCES rcbms_logs('log_id')
    );"
  )

  rcbms_private_keys <- glue::glue(
    "CREATE TABLE IF NOT EXISTS rcbms_private_keys (
      id INTEGER PRIMARY KEY,
      key_id VARCHAR(36) NOT NULL,
      log_id VARCHAR(36) NOT NULL,
      user_id VARCHAR(36) NOT NULL,
      region_code VARCHAR(2),
      province_code VARCHAR(3),
      station VARCHAR(32) NOT NULL,
      type TINYINT NOT NULL,
      name VARCHAR(128) NOT NULL,
      description TEXT,
      key TEXT NOT NULL,
      key_admin TEXT NOT NULL,
      status TINYINT DEFAULT 0,
      created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
      deleted_at DATETIME DEFAULT NULL,
      FOREIGN KEY('log_id') REFERENCES rcbms_logs('log_id')
    );"
  )

  if(wal_mode) {
    DBI::dbExecute(conn, "PRAGMA journal_mode = WAL;")
  }

  DBI::dbBegin(conn)
  DBI::dbExecute(conn, rcbms_logs)
  DBI::dbExecute(conn, rcbms_remarks)
  DBI::dbExecute(conn, rcbms_results)
  DBI::dbExecute(conn, rcbms_private_keys)

  DBI::dbExecute(conn, 'CREATE UNIQUE INDEX IF NOT EXISTS log_id_index on rcbms_logs (log_id);')
  DBI::dbExecute(conn, 'CREATE INDEX IF NOT EXISTS rcbms_logs_user_id_index on rcbms_logs (user_id);')
  DBI::dbExecute(conn, 'CREATE INDEX IF NOT EXISTS rcbms_logs_type_index on rcbms_logs (mode, user_id);')
  DBI::dbExecute(conn, 'CREATE INDEX IF NOT EXISTS rcbms_logs_type_status_index on rcbms_logs (mode, user_id, status);')

  DBI::dbExecute(conn, 'CREATE INDEX IF NOT EXISTS rcbms_results_log_id_index on rcbms_results (log_id);')
  DBI::dbExecute(conn, 'CREATE INDEX IF NOT EXISTS rcbms_results_validation_id_index on rcbms_results (mode_id);')
  DBI::dbExecute(conn, 'CREATE INDEX IF NOT EXISTS rcbms_results_mode_id_index on rcbms_results (input_data_id);')
  DBI::dbExecute(conn, 'CREATE INDEX IF NOT EXISTS rcbms_results_pk_index on rcbms_results (input_data_id, mode_id, line_number);')

  DBI::dbExecute(conn, 'CREATE INDEX IF NOT EXISTS rcbms_remarks_pk_index on rcbms_remarks (input_data_id, mode_id, line_number);')
  DBI::dbExecute(conn, 'CREATE INDEX IF NOT EXISTS rcbms_remarks_user_pk_index on rcbms_remarks (user_id, input_data_id, mode_id, line_number);')
  DBI::dbExecute(conn, 'CREATE INDEX IF NOT EXISTS rcbms_remarks_bulk_id_index on rcbms_remarks (bulk_id);')
  DBI::dbExecute(conn, 'CREATE INDEX IF NOT EXISTS rcbms_remarks_user_bulk_id_index on rcbms_remarks (user_id, bulk_id);')
  DBI::dbExecute(conn, 'CREATE INDEX IF NOT EXISTS rcbms_remarks_user_id_index on rcbms_remarks (user_id);')
  DBI::dbExecute(conn, 'CREATE INDEX IF NOT EXISTS rcbms_remarks_user_status_index on rcbms_remarks (user_id, status);')
  DBI::dbExecute(conn, 'CREATE INDEX IF NOT EXISTS rcbms_remarks_status_index on rcbms_remarks (status);')

  DBI::dbExecute(conn, 'CREATE INDEX IF NOT EXISTS rcbms_private_keys_log_id_index on rcbms_private_keys (log_id);')
  DBI::dbExecute(conn, 'CREATE INDEX IF NOT EXISTS rcbms_private_keys_type_index on rcbms_private_keys (type);')
  DBI::dbExecute(conn, 'CREATE INDEX IF NOT EXISTS rcbms_private_keys_user_index on rcbms_private_keys (user_id);')
  DBI::dbExecute(conn, 'CREATE INDEX IF NOT EXISTS rcbms_private_keys_user_type_index on rcbms_private_keys (user_id, type);')
  DBI::dbExecute(conn, 'CREATE INDEX IF NOT EXISTS rcbms_private_keys_type_region_province_index on rcbms_private_keys (province_code, region_code, type);')
  DBI::dbExecute(conn, 'CREATE INDEX IF NOT EXISTS rcbms_private_keys_type_province_index on rcbms_private_keys (province_code, type);')
  DBI::dbExecute(conn, 'CREATE INDEX IF NOT EXISTS rcbms_private_keys_type_region_index on rcbms_private_keys (region_code, type);')

  DBI::dbCommit(conn)

  DBI::dbDisconnect(conn, force = T)

  return(1)

}








