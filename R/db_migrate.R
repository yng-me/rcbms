#' Title
#'
#' @param .migrations
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'

db_migrate <- function(.migrations, ..., .name = NULL, .prefix = '') {
  db_conn <- db_connect()

  if(.prefix != '') prefix <- paste0(.prefix, '_')
  else prefix <- ''

  if(inherits(.migrations, 'list')) {

    db_tables <- names(.migrations)

    for(i in seq_along(db_tables)) {

      DBI::dbWriteTable(
        conn = db_conn,
        name = paste0(prefix, db_tables[i]),
        value = .migrations[i],
        row.names = F,
        ...
      )
    }

  } else {

    if(is.null(.name)) {
      stop('Table name is required')
    }

    DBI::dbWriteTable(
      conn = db_conn,
      paste0(prefix, .name),
      value = .migrations,
      row.names = F,
      ...
    )

  }
}


#' Title
#'
#' @param .env
#' @param ...
#'
#' @return
#' @export
#'
#' @examples

db_connect <- function(
  .env = getOption('rcbms.config')$env,
  .stage = 'dev',
  .db_name = NULL,
  ...
) {

  if(is.null(.env)) stop('Environment variable not provided.')
  if(!(.stage %in% c('dev', 'qa', 'test', 'prod', ''))) .stage <- 'dev'

  if(.stage == '') stage <- ''
  else stage <- paste0(.stage, '_')

  if(is.null(.db_name)) {
    .db_name <- .env[[paste0(toupper(stage), 'DB_DATABASE')]]
  }

  db_connection <- DBI::dbConnect(
    RMySQL::MySQL(),
    port = as.integer(.env$DB_PORT),
    dbname = .db_name,
    host = .env[[paste0(toupper(stage), 'DB_HOST')]],
    user = .env[[paste0(toupper(stage), 'DB_USERNAME')]],
    password = .env[[paste0(toupper(stage), 'DB_PASSWORD')]],
    ...
  )

  DBI::dbSendQuery(db_connection, "SET GLOBAL local_infile = true;")

  return(db_connection)

}
