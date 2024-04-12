#' Title
#'
#' @param .output
#' @param ...
#' @param .config
#' @param .name
#' @param .prefix
#' @param .suffix
#' @param .add_primary_key
#'
#' @return
#' @export
#'
#' @examples
#'
db_migrate <- function(
  .output,
  ...,
  .config = getOption("rcbms.config"),
  .name = NULL,
  .prefix = "",
  .suffix = "",
  .add_primary_key = TRUE
) {

  db_conn <- db_connect()

  if (.prefix != "") {
    prefix <- paste0(.prefix, "_")
  } else {
    prefix <- ""
  }

  if (.suffix != "") {
    suffix <- paste0("_", .suffix)
  } else {
    suffix <- ""
  }

  table_ids <- .name
  tb_overwrite <- rlang::list2(...)$overwrite
  if (is.null(tb_overwrite)) tb_overwrite <- FALSE

  add_id_column <- function(.tb_name, .with_survey_round_col = F) {

    if(.add_primary_key) {
      DBI::dbExecute(
        db_conn,
        paste0(
          "ALTER TABLE ",
          .tb_name,
          " ADD COLUMN `id` int(10) unsigned PRIMARY KEY AUTO_INCREMENT FIRST;"
        )
      )
    }

    if(.with_survey_round_col) {
      DBI::dbExecute(
        conn = db_conn,
        paste0(
          "ALTER TABLE ",
          .tb_name,
          " MODIFY survey_round YEAR;"
        )
      )
    }
  }

  if (inherits(.output, "rcbms_ts_list")) {
    db_tables <- names(.output)
    table_ids <- db_tables

    for (i in seq_along(db_tables)) {
      ts <- .output[[i]] |> dplyr::tibble()
      ts_name <- paste0(prefix, db_tables[i], suffix)

      DBI::dbWriteTable(
        conn = db_conn,
        name = ts_name,
        value = ts,
        ...,
        row.names = F
      )

      add_id_column(ts_name, "survey_round" %in% names(ts))

    }

  } else {
    if (is.null(.name)) {
      stop("Table name is required")
    }

    ts <- .output |> dplyr::tibble()
    tb_name <- paste0(prefix, .name, suffix)

    DBI::dbWriteTable(
      conn = db_conn,
      name = tb_name,
      value = ts,
      ...,
      row.names = F
    )

    add_id_column(tb_name, "survey_round" %in% names(ts))

  }

  suppressWarnings(DBI::dbDisconnect(db_conn, shutdown = T))
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
  .config = getOption("rcbms.config"),
  .db_name = NULL,
  ...
) {

  env <- .config$env
  stage <- .config$portal$stage
  local_infile <- .config$portal$db_migration$local_infile

  if (is.null(env)) stop("Environment variable not provided.")
  if (!(stage %in% c("dev", "qa", "test", "prod", ""))) stage <- "dev"

  if (stage == "") {
    stage <- ""
  } else {
    stage <- paste0(stage, "_")
  }

  if (is.null(.db_name)) {
    .db_name <- env[[paste0(toupper(stage), "DB_DATABASE")]]
  }

  db_connection <- DBI::dbConnect(
    RMySQL::MySQL(),
    port = as.integer(env$DB_PORT),
    dbname = .db_name,
    host = env[[paste0(toupper(stage), "DB_HOST")]],
    user = env[[paste0(toupper(stage), "DB_USERNAME")]],
    password = env[[paste0(toupper(stage), "DB_PASSWORD")]],
    ...
  )

  if (local_infile) {
    DBI::dbSendQuery(db_connection, "SET GLOBAL local_infile = true;")
  }

  return(db_connection)

}


#' Title
#'
#' @param refs
#'
#' @return
#' @export
#'
#' @examples
#'
db_migrate_refs <- function(refs = c("data_dictionary", "macrodata", "score_card"), table_names, ..., .include_area_names = T) {

  conn <- db_connect()

  for(i in seq_along(refs)) {
    db_migrate_ref(conn, refs[i], ...)
  }

  if(.include_area_names) {
    anm <- tidy_area_name(load_references("anm"), .add_length = 0) |>
      tibble::tibble()

    DBI::dbWriteTable(
      conn = conn,
      name = "area_names",
      value = anm,
      ...,
      row.names = F
    )
  }

  suppressWarnings(DBI::dbDisconnect(conn, shutdown = T))
}


db_migrate_ref <- function(.conn, .ref, ...) {

  gid <- gid_references() |>
    dplyr::filter(ref == .ref) |>
    dplyr::pull(gid)

  load_reference_fn <- eval(as.name(paste0("load_", .ref, "_refs")))

  if(.ref == "data_dictionary") {
    ref_df <- load_reference_fn(gid[1], FALSE)
    table_name <- .ref

    ref_df <- ref_df |>
      dplyr::filter(is_included == 1) |>
      dplyr::select(
        survey_round,
        input_data,
        variable_name = variable_name_new,
        item,
        sub_item,
        label,
        valueset,
        type,
        length,
        privacy_level
      )

  } else {
    ref_df <- load_reference_fn(gid[1])

    if(.ref == "macrodata") {

      table_name <- "stat_tables"
      ref_df <- ref_df |>
        dplyr::filter(table_name %in% table_names) |>
        dplyr::mutate(meta = jsonlite::toJSON(meta))

    } else if(.ref == "score_card") {
      table_name <- "score_cards"
    }
  }

  DBI::dbWriteTable(
    conn = .conn,
    name = table_name,
    value = ref_df,
    ...,
    row.names = F
  )

  if(.ref == "data_dictionary") {
    DBI::dbExecute(
      conn = .conn,
      "ALTER TABLE data_dictionary MODIFY survey_round YEAR;"
    )
  }

}
