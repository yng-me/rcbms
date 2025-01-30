alter_db_tables <- function(.conn, .input_data, .uid, .config = getOption('rcbms.config')) {

  indexes <- DBI::dbGetQuery(.conn, "PRAGMA index_list('logs');")
  if(nrow(indexes) > 1 & 'name' %in% names(indexes)) {

    log_id_index <- indexes$name[indexes$name == 'log_id_index']
    if(length(log_id_index) > 0) {
      return(invisible(NULL))
    }
  }

  db_name <- DBI::dbGetInfo(.conn)$dbname

  if(!is.null(.config$user_id) & !is.null(db_name)) {

    if(file.exists(db_name)) {

      dir_migration <- create_new_folder(file.path("..", "logs", "migration", .config$user_id))
      file.copy(db_name, dir_migration, overwrite = T)

    }
  }

  query <- create_table_query(
    .input_data = .input_data,
    .uid = .uid,
    .suffix = '_temp'
  )

  DBI::dbExecute(.conn, 'PRAGMA foreign_keys = OFF;')
  DBI::dbBegin(.conn)
  DBI::dbExecute(.conn, query$logs)
  DBI::dbExecute(.conn, 'INSERT INTO logs_temp SELECT * FROM logs;')
  DBI::dbExecute(.conn, 'DROP TABLE logs;')
  DBI::dbExecute(.conn, 'ALTER TABLE logs_temp RENAME TO logs;')

  if(.input_data == 'bp') {
    bp_cv_fields <- DBI::dbListFields(.conn, glue::glue('{.input_data}_cv'))

    if(!('tag_status' %in% bp_cv_fields)) {
      DBI::dbExecute(.conn, glue::glue('ALTER TABLE {.input_data}_cv ADD COLUMN tag_status DATETIME DEFAULT NULL'))
    }
    if(!('updated_at' %in% bp_cv_fields)) {
      DBI::dbExecute(.conn, glue::glue('ALTER TABLE {.input_data}_cv ADD COLUMN updated_at DATETIME DEFAULT NULL'))
    }
  }

  DBI::dbExecute(.conn, query$cv)
  DBI::dbExecute(.conn, glue::glue('INSERT OR IGNORE INTO {.input_data}_cv_temp SELECT * FROM {.input_data}_cv;'))
  DBI::dbExecute(.conn, glue::glue('DROP TABLE {.input_data}_cv;'))
  DBI::dbExecute(.conn, glue::glue('ALTER TABLE {.input_data}_cv_temp RENAME TO {.input_data}_cv;'))

  DBI::dbExecute(.conn, query$ts)
  DBI::dbExecute(
    .conn,
    'INSERT OR IGNORE INTO ts_temp
    SELECT
      id,
      log_id,
      tabulation_id,
      summary,
      info,
      status,
      tag_status,
      updated_at
    FROM ts;'
  )
  DBI::dbExecute(.conn, 'DROP TABLE ts;')
  DBI::dbExecute(.conn, 'ALTER TABLE ts_temp RENAME TO ts;')

  DBI::dbExecute(.conn, query$remarks)
  DBI::dbExecute(.conn, glue::glue('INSERT OR IGNORE INTO remarks_temp SELECT * FROM remarks;'))
  DBI::dbExecute(.conn, glue::glue('DROP TABLE remarks;'))
  DBI::dbExecute(.conn, glue::glue('ALTER TABLE remarks_temp RENAME TO remarks;'))

  create_logs_index(.conn)
  create_remarks_index(.conn)
  create_ts_index(.conn)
  create_cv_index(.conn, .input_data, .uid)

  DBI::dbCommit(.conn)
  DBI::dbExecute(.conn, 'PRAGMA foreign_keys = ON;')

}

