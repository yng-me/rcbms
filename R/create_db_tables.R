create_db_tables <- function(.conn, .input_data, .uid) {

  query <- create_table_query(.input_data, .uid)

  DBI::dbBegin(.conn)
  DBI::dbExecute(.conn, query$logs)
  DBI::dbExecute(.conn, query$cv)
  DBI::dbExecute(.conn, query$ts)
  DBI::dbExecute(.conn, query$remarks)

  DBI::dbExecute(.conn, 'CREATE INDEX remarks_uuid_index on remarks (uuid);')
  DBI::dbExecute(.conn, 'CREATE INDEX remarks_user_id_index on remarks (user_id);')
  DBI::dbExecute(.conn, 'CREATE INDEX remarks_status_index on remarks (status);')

  DBI::dbExecute(.conn, glue::glue('CREATE INDEX cv_log_id_index on {.input_data}_cv (log_id);'))
  DBI::dbExecute(.conn, glue::glue('CREATE INDEX cv_validation_id_index on {.input_data}_cv (validation_id);'))
  DBI::dbExecute(.conn, glue::glue('CREATE INDEX cv_{.uid}_index on {.input_data}_cv ({.uid});'))
  DBI::dbExecute(.conn, glue::glue('CREATE INDEX cv_status_index on {.input_data}_cv (status);'))

  DBI::dbExecute(.conn, 'CREATE INDEX ts_log_id_index on ts (log_id);')
  DBI::dbExecute(.conn, 'CREATE INDEX ts_tabulation_id_index on ts (tabulation_id);')
  DBI::dbExecute(.conn, 'CREATE INDEX ts_status_index on ts (status);')

  DBI::dbExecute(.conn, 'CREATE UNIQUE INDEX log_id_index on logs (id);')
  DBI::dbExecute(.conn, 'CREATE INDEX log_user_id_index on logs (user_id);')
  DBI::dbExecute(.conn, 'CREATE INDEX log_type_index on logs (input_data, mode, user_id);')
  DBI::dbExecute(.conn, 'CREATE INDEX log_type_status_index on logs (input_data, mode, user_id, status);')

  DBI::dbCommit(.conn)

  print('asdfsadfsadf')

}

create_table_query <- function(.input_data, .uid, .suffix = '') {

  # Logs
  # --------------------
  q_log <- glue::glue(
    "CREATE TABLE IF NOT EXISTS logs{.suffix} (
      id_int INTEGER,
      id VARCHAR(36) UNIQUE,
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
      total_cases INTEGER,
      total_cases_unique INTEGER,
      total_priority_a INTEGER,
      total_priority_b INTEGER,
      total_priority_c INTEGER,
      total_priority_d INTEGER,
      total INTEGER,
      summary TEXT,
      user TEXT,
      duration INTEGER,
      source TINYINT,
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
      validated_at DATETIME DEFAULT NULL,
      PRIMARY KEY('id')
    );"
  )


  # CV
  # --------------------
  line_number <- ''
  contact <- ''
  q_unique <- glue::glue("UNIQUE('id', 'log_id', '{.uid}', 'validation_id')")
  q_primary <- glue::glue("PRIMARY KEY('id', 'log_id', '{.uid}', 'validation_id')")

  if(.input_data == 'hp' | .input_data == 'ilq') {
    line_number <- 'line_number VARCHAR(4),'
    contact <- 'contact TEXT,'
    q_unique <- glue::glue("UNIQUE('id', 'log_id', '{.uid}', 'validation_id', 'line_number')")
    q_primary <- glue::glue("PRIMARY KEY('id', 'log_id', '{.uid}', 'validation_id', 'line_number')")
  }

  q_cv <- glue::glue(
    "CREATE TABLE IF NOT EXISTS {.input_data}_cv{.suffix} (
        id VARCHAR(36),
        log_id VARCHAR(36),
        {.uid} VARCHAR(36),
        validation_id VARCHAR(128),
        {line_number}
        status TINYINT,
        info TEXT,
        {contact}
        tag_status DATETIME DEFAULT NULL,
        updated_at DATETIME DEFAULT NULL,
        FOREIGN KEY('log_id') REFERENCES logs('id'),
        {q_primary}
      );"
  )


  # TS
  # --------------------
  q_ts <- glue::glue(
    "CREATE TABLE IF NOT EXISTS ts{.suffix} (
      id VARCHAR(36),
      log_id VARCHAR(36),
      tabulation_id VARCHAR(36),
      area_code VARCHAR(36),
      summary TEXT,
      info TEXT,
      status TINYINT,
      tag_status DATETIME DEFAULT NULL,
      updated_at DATETIME DEFAULT NULL,
      FOREIGN KEY('log_id') REFERENCES logs('id'),
      PRIMARY KEY('id', 'log_id', 'tabulation_id')
    );"
  )


  # Remarks
  # --------------------
  q_remarks <- glue::glue("
    CREATE TABLE IF NOT EXISTS remarks{.suffix} (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      uuid VARCHAR(36),
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
      updated_at DATETIME DEFAULT NULL,
      FOREIGN KEY('uuid') REFERENCES {.input_data}_cv('id'),
      FOREIGN KEY('uuid') REFERENCES ts('id')
    );
  ")

  return(
    list(
      cv = q_cv,
      ts = q_ts,
      logs = q_log,
      remarks = q_remarks
    )
  )

}
