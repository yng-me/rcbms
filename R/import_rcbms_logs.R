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

  # .dir <- '/Users/bhasabdulsamad/Library/Application Support/ph.gov.psa.rcbms.app/logs'

  exdir <- file.path(.dir, 'db-temp')

  db_logs <- list.files(
    exdir,
    recursive = T,
    full.names = T,
    pattern = '(hp|bp|ilq|shp)_rcbms_logs_v.*db$'
  )

  for(i in seq_along(db_logs)) {

    # read logs from other user
    db_log_i <- db_logs[i]
    db_i <- basename(db_log_i)
    input_data <- stringr::str_extract(db_i, '^(hp|bp|ilq|shp)')
    db_cv <- paste0(input_data, '_cv')

    conn_i <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_log_i)

    # per db table
    logs_i <- DBI::dbReadTable(conn_i, 'logs') |>
      dplyr::filter(status > 0)

    if(nrow(logs_i) == 0) {
      unlink(exdir, recursive = T, force = T)
      print('empty')
      return(NULL)
    }

    remarks_i <- DBI::dbReadTable(conn_i, 'remarks') |>
      dplyr::filter(status > 0)

    ts_i <- DBI::dbReadTable(conn_i, 'ts') |>
      dplyr::filter(as.integer(status) > 0)

    cv_i <- DBI::dbReadTable(conn_i, db_cv) |>
      dplyr::filter(as.integer(status) > 0)

    DBI::dbDisconnect(conn_i)

    uid <- 'case_id'
    by_cv_cols <- c(uid, "validation_id", "line_number")

    if(input_data == 'shp') {
      uid <- 'cbms_geoid'
      by_cv_cols <- c(uid, "validation_id")
    } else if(input_data == 'bp') {
      uid <- 'barangay_geo'
      by_cv_cols <- c(uid, "validation_id")
    }

    # add logs to current user
    db_dir <- rcbms::create_new_folder(file.path(.dir, 'db', .user_id))

    conn <- DBI::dbConnect(
      RSQLite::SQLite(),
      dbname = file.path(db_dir, db_i)
    )

    db_tables <- DBI::dbListTables(conn)

    create_logs_table(conn, db_tables, input_data, uid)
    create_remarks_table(conn, db_tables)

    # import logs
    if(nrow(logs_i) > 0 ) {

      id_int <- DBI::dbReadTable(conn, 'logs') |>
        nrow()

      DBI::dbWriteTable(
        conn,
        value = logs_i |>
          dplyr::mutate(
            id_int = id_int + (1:dplyr::n())
          ),
        name = 'logs',
        append = T
      )
    }

    # import cv
    if(nrow(cv_i) > 0) {
      DBI::dbWriteTable(
        conn,
        value = cv_i,
        name = db_cv,
        append = T
      )
    }

    # import ts
    if(nrow(ts_i) > 0) {
      DBI::dbWriteTable(
        conn,
        value = ts_i,
        name = 'ts',
        append = T
      )
    }

    # import remarks
    if(nrow(remarks_i) > 0) {

      remarks_i_join <- remarks_i |>
        dplyr::left_join(
          cv_i |>
            dplyr::select(
              uuid = id,
              dplyr::any_of(by_cv_cols)
            ),
          by = 'uuid')

      cv <- DBI::dbReadTable(conn, db_cv)

      if(nrow(cv) > 0) {

        if(input_data == 'bp' | input_data == 'shp') {

          remarks_i_join <- remarks_i_join |>
            dplyr::filter(
              !!as.name(uid) %in% cv[[uid]],
              validation_id %in% cv$validation_id
            )

        } else {

          remarks_i_join <- remarks_i_join |>
            dplyr::filter(
              case_id %in% cv$case_id,
              line_number %in% cv$line_number,
              validation_id %in% cv$validation_id
            )
        }

        if(nrow(remarks_i_join) > 0) {

          remarks_i_join <- remarks_i_join |>
            dplyr::select(-uuid) |>
            dplyr::left_join(
              cv |>
                dplyr::select(
                  uuid = id,
                  dplyr::any_of(by_cv_cols)
                ),
              by = by_cv_cols
            ) |>
            dplyr::select(-dplyr::any_of(by_cv_cols)) |>
            dplyr::distinct(
              uuid,
              user_id,
              status,
              remarks,
              .keep_all = T
            )

          DBI::dbWriteTable(
            conn,
            value = remarks_i_join |>
              dplyr::select(-id),
            name = 'remarks',
            append = T
          )

          remarks_i <- remarks_i |>
            dplyr::filter(!(id %in% remarks_i_join$id))
        }
      }

      DBI::dbWriteTable(
        conn,
        value = remarks_i |> dplyr::select(-id),
        name = 'remarks',
        append = T
      )
    }

    DBI::dbDisconnect(conn)
  }

  unlink(exdir, recursive = T, force = T)
  print('success')

}
