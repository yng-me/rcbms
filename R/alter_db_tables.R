alter_db_tables <- function(.conn, .input_data, .uid, .config = getOption('rcbms.config')) {

  indexes <- DBI::dbGetQuery(.conn, "PRAGMA index_list('logs');")
  if(nrow(indexes) > 1) return(invisible(NULL))

  db_name <- DBI::dbGetInfo(.conn)$dbname

  if(!is.null(.config$user_id) & !is.null(db_name)) {

    if(file.exists(db_name)) {

      dir_migration <- create_new_folder(file.path("..", "logs", "migration", .config$user_id))
      file.copy(db_name, dir_migration, overwrite = T)
    }
  }

  query <- create_table_query(
    .input_data = 'hp',
    .uid = 'case_id',
    .suffix = '_temp'
  )

  DBI::dbExecute(.conn, 'PRAGMA foreign_keys = OFF;')
  DBI::dbBegin(.conn)
  DBI::dbExecute(.conn, query$logs)
  DBI::dbExecute(.conn, 'INSERT INTO logs_temp SELECT * FROM logs;')
  DBI::dbExecute(.conn, 'DROP TABLE logs;')
  DBI::dbExecute(.conn, 'ALTER TABLE logs_temp RENAME TO logs;')

  DBI::dbExecute(.conn, query$cv)
  DBI::dbExecute(
    .conn,
    glue::glue('INSERT OR IGNORE INTO {.input_data}_cv_temp SELECT * FROM {.input_data}_cv;')
  )
  DBI::dbExecute(.conn, glue::glue('DROP TABLE {.input_data}_cv;'))
  DBI::dbExecute(.conn, glue::glue('ALTER TABLE {.input_data}_cv_temp RENAME TO {.input_data}_cv;'))

  DBI::dbExecute(.conn, query$ts)
  DBI::dbExecute(
    .conn,
    'INSERT OR IGNORE INTO ts_temp SELECT * FROM ts;'
  )
  DBI::dbExecute(.conn, 'DROP TABLE ts;')
  DBI::dbExecute(.conn, 'ALTER TABLE ts_temp RENAME TO ts;')

  DBI::dbExecute(.conn, query$remarks)
  DBI::dbExecute(
    .conn,
    glue::glue('INSERT OR IGNORE INTO remarks_temp SELECT * FROM remarks;')
  )
  DBI::dbExecute(.conn, glue::glue('DROP TABLE remarks;'))
  DBI::dbExecute(.conn, glue::glue('ALTER TABLE remarks_temp RENAME TO remarks;'))

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
  DBI::dbExecute(.conn, 'PRAGMA foreign_keys = ON;')

}
