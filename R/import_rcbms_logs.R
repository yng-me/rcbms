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
    pattern = '(hp|bp|ilq|shp)_rcbms_logs_v.*db$'
  )

  done <- NULL
  status <- 'empty'

  db_dir <- file.path(.dir, 'db', .user_id)

  for(i in seq_along(db_logs)) {
    extract_rcbms_log(db_dir, db_logs[i])
  }

  unlink(exdir, recursive = T, force = T)
  print('success')

}





extract_rcbms_log <- function(.dir, db_log_i) {

  # read logs from other user
  db_i <- basename(db_log_i)
  input_data <- stringr::str_extract(db_i, '^(hp|bp|ilq|shp)')
  db_cv <- paste0(input_data, '_cv')
  uid_cols <- get_uid_cols(input_data)

  conn_i <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_log_i)

  if('logs' %in% DBI::dbListTables(conn_i)) {

    # per db table
    logs_i <- DBI::dbReadTable(conn_i, 'logs') |>
      dplyr::filter(status > 0)

    if(nrow(logs_i) > 0) {

      # add logs to current user
      db_dir <- rcbms::create_new_folder(.dir)

      conn <- DBI::dbConnect(
        RSQLite::SQLite(),
        dbname = file.path(db_dir, db_i)
      )

      db_tables <- DBI::dbListTables(conn)

      create_logs_table(conn, db_tables, input_data, uid_cols)
      create_remarks_table(conn, db_tables)

      # import logs
      o_logs <- DBI::dbReadTable(conn, 'logs') |>
        dplyr::select(id)

      s_logs_i <- logs_i |>
        dplyr::filter(!(id %in% o_logs$id))

      if(nrow(s_logs_i) > 0) {

        ts_i <- DBI::dbReadTable(conn_i, 'ts') |>
          dplyr::filter(as.integer(status) > 0) |>
          dplyr::filter(!(log_id %in% o_logs$id))

        cv_i <- DBI::dbReadTable(conn_i, db_cv) |>
          dplyr::filter(as.integer(status) > 0) |>
          dplyr::filter(!(log_id %in% o_logs$id))

        remarks_i <- DBI::dbReadTable(conn_i, 'remarks') |>
          dplyr::filter(status > 0) |>
          dplyr::filter(uuid %in% cv_i$id | uuid %in% ts_i$id)

        DBI::dbDisconnect(conn_i)

        DBI::dbWriteTable(
          conn,
          value = s_logs_i |>
            dplyr::mutate(id_int = nrow(o_logs) + (1:dplyr::n())),
          name = 'logs',
          append = T
        )

        # import cv
        if(nrow(cv_i) > 0) {

          remarks_i <- import_rcbms_log(
            .conn = conn,
            .data = cv_i,
            .remarks = remarks_i,
            .input_data = input_data,
            .name = db_cv
          )
        }

        # import ts
        if(nrow(ts_i) > 0) {

          remarks_i <- import_rcbms_log(
            .conn = conn,
            .data = ts_i,
            .remarks = remarks_i,
            .input_data = input_data,
            .name = 'ts'
          )
        }

        # import remaining remarks
        if(nrow(remarks_i) > 0) {

          DBI::dbWriteTable(
            conn,
            value = remarks_i |> dplyr::select(-id),
            name = 'remarks',
            append = T
          )

        }

      }

      DBI::dbDisconnect(conn)

    }
  }

}
