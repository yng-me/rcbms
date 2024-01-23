#' Title
#'
#' @param .output
#' @param .name
#' @param .prefix
#' @param .add_primary_key
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'

db_migrate <- function(
  .output,
  ...,
  .name = NULL,
  .prefix = '',
  .add_primary_key = TRUE
) {

  db_conn <- db_connect()

  if(.prefix != '') prefix <- paste0(.prefix, '_')
  else prefix <- ''

  if(inherits(.output, 'rcbms_ts_list')) {

    db_tables <- names(.output)

    for(i in seq_along(db_tables)) {

      ts <- .output[i]
      if(inherits(ts, 'rcbms_ts_tbl')) {

        DBI::dbWriteTable(
          conn = db_conn,
          name = paste0(prefix, db_tables[i]),
          value = ts,
          row.names = F,
          ...
        )

        DBI::dbSendQuery(
          db_conn,
          paste0(
            'ALTER TABLE ',
            prefix, db_tables[i],
            ' ADD COLUMN `id` int(10) unsigned PRIMARY KEY AUTO_INCREMENT FIRST;'
          )
        )
      }
    }

  } else {

    if(is.null(.name)) {
      stop('Table name is required')
    }

    DBI::dbWriteTable(
      conn = db_conn,
      paste0(prefix, .name),
      value = .output,
      row.names = F,
      ...
    )

    DBI::dbSendQuery(
      db_conn,
      paste0(
        'ALTER TABLE ',
        prefix, .name,
        ' ADD COLUMN `id` int(10) unsigned PRIMARY KEY AUTO_INCREMENT FIRST;'
      )
    )
#
#     DBI::dbWriteTable(
#       conn = db_conn,
#       name = "stat_tables",
#       value = ,
#       row.names = F,
#       ...
#     )


  }

  suppressWarnings(DBI::dbDisconnect(db_conn))

}


#' Title
#'
#' @param .config
#' @param .db_name
#' @param ...
#'
#' @return
#' @export
#'
#' @examples

db_connect <- function(
  .config = getOption('rcbms.config'),
  .db_name = NULL,
  ...
) {
  env <- .config$env
  stage <- .config$portal$stage

  if(is.null(env)) stop('Environment variable not provided.')
  if(!(stage %in% c('dev', 'qa', 'test', 'prod', ''))) stage <- 'dev'

  if(stage == '') stage <- ''
  else stage <- paste0(stage, '_')

  if(is.null(.db_name)) {
    .db_name <- env[[paste0(toupper(stage), 'DB_DATABASE')]]
  }

  db_connection <- DBI::dbConnect(
    RMySQL::MySQL(),
    port = as.integer(env$DB_PORT),
    dbname = .db_name,
    host = env[[paste0(toupper(stage), 'DB_HOST')]],
    user = env[[paste0(toupper(stage), 'DB_USERNAME')]],
    password = env[[paste0(toupper(stage), 'DB_PASSWORD')]],
    ...
  )

  DBI::dbSendQuery(db_connection, "SET GLOBAL local_infile = true;")

  return(db_connection)

}
