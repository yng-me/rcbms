save_rcbms_logs <- function(.data, .input_data, .references, .config) {

  conn <- connect_to_rcbms_logs_db(.config, .input_data)
  log_tables <- DBI::dbListTables(conn)

  create_logs_table(conn, log_tables)
  save_current_logs(conn, .data, .input_data, log_tables, .references, .config)

  create_remarks_table(conn, log_tables)

  DBI::dbDisconnect(conn)

}

save_current_logs <- function(
  .conn,
  .data,
  .input_data,
  .tables,
  .references,
  .config
) {


  pc <- Sys.info()
  os_release_date <- pc[["version"]] |>
    stringr::str_extract(":\\s\\w*\\s\\w*\\s\\d{2}\\s\\d{2}:\\d{2}:\\d{2}.*;") |>
    stringr::str_remove("^:\\s") |>
    stringr::str_remove(";$")

  db_data_to_store <- NULL
  current_area_code <- .config$aggregation$areas[1]
  uid <- .config$project[[.input_data]]$id
  current_id <- 1
  log_status <- 2
  total_cases <- 0
  total_cases_unique <- 0
  total_priority_a <- 0
  total_priority_b <- 0
  total_priority_c <- 0
  total_priority_d <- 0

  mode <- .config$mode$type

  if(!is.null(.data)) {
    db_data_to_store <- .data |>
      add_uuid(.id_name = "id")
  }

  if((mode == "validation" | mode == "cv") & !is.null(.data))  {

    db_table_name <- paste0(.input_data, "_cv")

    if (.input_data %in% c("hp", "ilq")) {
      by_cv_cols <- c(uid, "validation_id", "line_number")
    } else if (.input_data %in% c("bp", "shp")) {
      by_cv_cols <- c(uid, "validation_id")
    }

    db_data_to_store <- db_data_to_store |>
      dplyr::select(dplyr::any_of(c("id", uid, "validation_id", "line_number", "info")))

    if (uid %in% names(db_data_to_store)) {
      total_cases_unique <- db_data_to_store |>
        dplyr::distinct(!!as.name(uid)) |>
        nrow()
    } else {
      total_cases_unique <- 0
    }

    priority_ref <- .references |>
      dplyr::select(
        validation_id,
        dplyr::any_of(c('priority_level', 'section'))
      )

    if(!("priority_level" %in% names(priority_ref))) {
      priority_ref <- priority_ref |>
        dplyr::mutate(priority_level = NA_character_)
    }

    if(!("section" %in% names(priority_ref))) {
      priority_ref <- priority_ref |>
        dplyr::mutate(section = NA_character_)
    }

    priority_df <- db_data_to_store |>
      dplyr::select(validation_id) |>
      dplyr::left_join(priority_ref, by = "validation_id") |>
      dplyr::mutate(priority_level = stringr::str_trim(tolower(priority_level)))

    get_priority <- function(.level) {
      priority_df |>
        dplyr::filter(priority_level == .level) |>
        nrow()
    }

    total_priority_a <- get_priority("a")
    total_priority_b <- get_priority("b")
    total_priority_c <- get_priority("c")
    total_priority_d <- get_priority("d")

    summary_info <- list(
      priority_level = priority_df |>
        dplyr::count(priority_level, name = 'value') |>
        dplyr::rename(key = priority_level),
      section = priority_df |>
        dplyr::count(section, name = 'value') |>
        dplyr::rename(key = section)
    )

    log_status <- 0
    total_cases <- nrow(db_data_to_store)

  }

  if((mode == "tabulation" | mode == "ts") & !is.null(.data)) {

    log_status <- 0

    db_table_name <- "ts"
    by_cv_cols <- "tabulation_id"

    db_data_to_store <- db_data_to_store |>
      dplyr::select(dplyr::any_of(c("id", "tabulation_id", "info")))

  }

  summary_stat <- create_summary_stat(.input_data, .config)

  total <- summary_stat$total
  summary_info <- summary_stat$summary_info

  verified_at <- NULL
  if(log_status == 2) verified_at <- Sys.time()

  log_saved <- DBI::dbWriteTable(
    conn = .conn,
    name = "logs",
    value = dplyr::tibble(
      survey_round = as.character(.config$survey_round),
      mode = mode,
      edit = .config$mode$edit,
      stage = .config$mode$stage,
      station = toupper(.config$mode$station),
      level = .config$aggregation$level,
      input_data = .input_data,
      area_code = current_area_code,
      total = total,
      summary = as.character(jsonlite::toJSON(summary_info)),
      total_cases = total_cases,
      total_cases_unique = total_cases_unique,
      total_priority_a = total_priority_a,
      total_priority_b = total_priority_b,
      total_priority_c = total_priority_c,
      total_priority_d = total_priority_d,
      version_app = .config$version$app,
      version_package = .config$version$package,
      version_script = .config$version$script,
      verified_at = verified_at,
      status = log_status,
      pc_os = tolower(pc[["sysname"]]),
      pc_user = pc[["user"]],
      pc_effective_user = pc[["effective_user"]],
      pc_os_release_date = os_release_date,
      pc_os_version = pc[["release"]],
      pc_hardware = pc[["machine"]],
      pc_pid = Sys.getpid()
    ),
    append = TRUE
  )

  if (log_saved & !is.null(.data)) {

    last_row <- DBI::dbGetQuery(.conn, "SELECT last_insert_rowid();")
    current_id <- as.integer(last_row[[1]])

    db_data_to_store <- db_data_to_store |>
      dplyr::mutate(log_id = current_id, status = 0L)

    if (db_table_name %in% .tables) {

      cv_logs_with_remarks <- DBI::dbReadTable(.conn, db_table_name) |>
        dplyr::tibble() |>
        dplyr::filter(as.integer(status) != 0) |>
        dplyr::mutate(status = as.integer(status)) |>
        dplyr::mutate(old_uuid = id) |>
        dplyr::select(old_uuid, status, dplyr::any_of(by_cv_cols))

      if (nrow(cv_logs_with_remarks) > 0) {
        db_data_to_store <- db_data_to_store |>
          dplyr::select(-status) |>
          dplyr::left_join(cv_logs_with_remarks, by = by_cv_cols, multiple = "first") |>
          dplyr::mutate(
            id = dplyr::if_else(is.na(old_uuid), id, old_uuid),
            status = dplyr::if_else(is.na(status), 0L, as.integer(status))
          ) |>
          dplyr::select(-dplyr::any_of("old_uuid"))
      }
    }

    DBI::dbWriteTable(
      conn = .conn,
      name = db_table_name,
      value = db_data_to_store,
      append = TRUE
    )

  }

  if (!exists("current_logs_id")) {
    current_logs_id <- NULL
  }
  current_logs_id <- c(current_logs_id, current_id)

  envir <- as.environment(1)
  assign("current_logs_id", current_logs_id, envir = envir)

  return(db_data_to_store)
}
