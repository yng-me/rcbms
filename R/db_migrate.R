#' Title
#'
#' @param .output
#' @param ...
#' @param .name
#' @param .prefix
#' @param .add_primary_key
#' @param .add_table_ref
#' @param .suffix
#' @param .references
#' @param .config
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
  .prefix = "",
  .suffix = "",
  .add_primary_key = TRUE,
  .add_table_ref = FALSE,
  .references = get_config("references"),
  .config = getOption("rcbms.config")
) {

  db_conn <- db_connect()

  if(.prefix != '') prefix <- paste0(.prefix, '_')
  else prefix <- ''

  if(.suffix != '') suffix <- paste0('_', .suffix)
  else suffix <- ''

  table_ids <- .name
  tb_overwrite <- rlang::list2(...)$overwrite
  if(is.null(tb_overwrite)) tb_overwrite <- FALSE

  if(inherits(.output, 'rcbms_ts_list')) {

    db_tables <- names(.output)
    table_ids <- db_tables

    for(i in seq_along(db_tables)) {

      ts <- .output[[i]] |> dplyr::tibble()
      ts_name <- paste0(prefix, db_tables[i], suffix)

      DBI::dbWriteTable(
        conn = db_conn,
        name = ts_name,
        value = ts,
        ...,
        row.names = F
      )

      if(.add_primary_key) {
        DBI::dbSendQuery(
          db_conn,
          paste0('ALTER TABLE ', ts_name, ' ADD COLUMN `id` int(10) unsigned PRIMARY KEY AUTO_INCREMENT FIRST;')
        )
      }

    }

    if(tb_overwrite) {

      add_stat_table_ref(db_conn, .references, table_ids, ..., .add_primary_key)
      add_score_card_ref(db_conn, .references, ..., .add_primary_key)

    }

  } else {

    if(is.null(.name)) {
      stop('Table name is required')
    }

    tb_name <- paste0(prefix, .name, suffix)

    DBI::dbWriteTable(
      conn = db_conn,
      name = tb_name,
      value = .output |> dplyr::tibble(),
      ...,
      row.names = F
    )

    if(.add_table_ref) {
      add_stat_table_ref(db_conn, .references, table_ids, append = T, .add_primary_key)
      add_score_card_ref(db_conn, .references, append = T, .add_primary_key)
    }

    if(.add_primary_key) {

      DBI::dbSendQuery(
        db_conn,
        paste0('ALTER TABLE ', tb_name, ' ADD COLUMN `id` int(10) unsigned PRIMARY KEY AUTO_INCREMENT FIRST;')
      )
    }
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
#'

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


add_stat_table_ref <- function(.conn, .references, .table_ids, ..., .add_primary_key = T) {

  if(!is.null(.references$macrodata)) {

    stat_tables <- .references$macrodata |>
      dplyr::filter(table_name %in% .table_ids) |>
      dplyr::distinct(table_name, category, .keep_all = T) |>
      dplyr::select(-dplyr::any_of(c("input_data", "survey_round")))

    DBI::dbWriteTable(
      conn = .conn,
      name = 'stat_tables',
      value = stat_tables,
      ...,
      row.names = F
    )

    if(.add_primary_key) {
      DBI::dbSendQuery(
        .conn,
        'ALTER TABLE stat_tables ADD COLUMN `id` int(10) unsigned PRIMARY KEY AUTO_INCREMENT FIRST;'
      )
    }
  }
}

add_score_card_ref <- function(.conn, .references, ..., .add_primary_key = T) {

  if(!is.null(.references$score_card)) {

    score_cards <- .references$score_card |>
      dplyr::distinct(variable_name, category, .keep_all = T) |>
      dplyr::select(-dplyr::any_of(c("input_data", "survey_round")))

    DBI::dbWriteTable(
      conn = .conn,
      name = 'score_cards',
      value = score_cards,
      ...,
      row.names = F
    )

    if(.add_primary_key) {
      DBI::dbSendQuery(
        .conn,
        'ALTER TABLE score_cards ADD COLUMN `id` int(10) unsigned PRIMARY KEY AUTO_INCREMENT FIRST;'
      )
    }
  }
}

