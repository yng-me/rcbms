save_rcbms_logs <- function(
  .data,
  .input_data,
  .references = get_config("references"),
  .config = getOption("rcbms.config")
) {

  conn <- connect_to_rcbms_logs(.config)
  log_tables <- RSQLite::dbListTables(conn)

  cv_table_current <- paste0(.input_data, "_cv_current")
  cv_table_name <- paste0(.input_data, "_cv")

  save_current_logs(conn, .data, .input_data, log_tables, .references, .config)
  create_remarks_table(conn, log_tables)

  RSQLite::dbDisconnect(conn)

}


connect_to_rcbms_logs <- function(.config) {
  wd <- create_new_folder(paste0(.config$base, "/data/log"))
  v <- config$version$db
  if(is.null(v)) v <- "0.0.1"
  db_name <- paste0(wd, "/rcbms_logs_v", v, ".db")
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), db_name)
  return(conn)
}


create_remarks_table <- function(.conn, .tables) {
  if(!("remarks" %in% .tables)) {
    DBI::dbExecute(
      .conn,
      "CREATE TABLE remarks (
        id varchar(36),
        user_id varchar(36),
        status tinyint CHECK (status IN (1, 2, 3, 4)),
        remarks text,
        created_at DATETIME DEFAULT CURRENT_TIMESTAMP
      );"
    )
  }
}

save_current_logs <- function(
  .conn,
  .data,
  .input_data,
  .tables,
  .references,
  .config
) {

  created_date <- lubridate::now()

  if(!exists('current_area_code')) {
    current_area_code <- ''
  }

  if(!("logs" %in% .tables)) {
    DBI::dbExecute(
      .conn,
      "CREATE TABLE logs (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        round varchar(4),
        mode varchar(16),
        edit tinyint CHECK (edit IN (1, 2, 3, 4)),
        level tinyint CHECK (level IN (1, 2, 3, 4, 5)),
        source tinyint CHECK (source IN (1, 2, 3)),
        station char(5) CHECK (station IN ('CO', 'RO', 'PO', 'LGU')),
        input_data char(3) CHECK (input_data IN ('hp', 'bp', 'ilq')),
        area_code varchar(16),
        total_cases int,
        total_cases_unique int,
        total_priority_a int,
        total_priority_b int,
        total_priority_c int,
        total_priority_d int,
        user_id varchar(36),
        version_app varchar(10),
        version_package varchar(10),
        version_script varchar(10),
        pc_user varchar(32),
        pc_effective_user varchar(32),
        pc_os varchar(16),
        pc_os_release_date varchar(36),
        pc_os_version varchar(36),
        pc_pid int,
        pc_hardware varchar(16),
        created_at DATETIME DEFAULT CURRENT_TIMESTAMP
      );"
    )
  }

  pc <- Sys.info()
  os_release_date <- pc[["version"]] |>
    stringr::str_extract(":\\s\\w*\\s\\w*\\s\\d{2}\\s\\d{2}:\\d{2}:\\d{2}.*;") |>
    stringr::str_remove("^:\\s") |>
    stringr::str_remove(";$")

  uid <- "case_id"
  if(.input_data == "bp") uid <- "uuid"
  current_id <- 1

  cv_data <- .data |>
    dplyr::select(dplyr::any_of(c("id", uid, "validation_id", "line_number"))) |>
    dplyr::mutate(log_id = current_id)

  total_cases_unique <- cv_data |>
    dplyr::distinct(!!as.name(uid)) |>
    nrow()

  cv_ref <- .references$validation |>
    dplyr::collect() |>
    dplyr::filter(
      cbms_round == as.integer(.config$cbms_round),
      input_data == .input_data
    ) |>
    dplyr::select(validation_id, priority_level)

  priority_df <- cv_data |>
    dplyr::select(validation_id) |>
    dplyr::left_join(
      .references$validation |>
        dplyr::collect() |>
        dplyr::filter(
          cbms_round == as.integer(.config$cbms_round),
          input_data == .input_data
        ) |>
        dplyr::select(validation_id, priority_level),
      by = "validation_id"
    ) |>
    dplyr::mutate(priority_level = stringr::str_trim(tolower(priority_level)))

  priority_a <- priority_df |>
    dplyr::filter(priority_level == "a") |>
    nrow()

  priority_b <- priority_df |>
    dplyr::filter(priority_level == "b") |>
    nrow()

  priority_c <- priority_df |>
    dplyr::filter(priority_level == "c") |>
    nrow()

  priority_d <- priority_df |>
    dplyr::filter(priority_level == "d") |>
    nrow()

  log_saved <- DBI::dbWriteTable(
    conn = .conn,
    name = "logs",
    value = dplyr::tibble(
      round = .config$cbms_round,
      mode = .config$mode$type,
      edit = .config$mode$edit,
      source = .config$mode$source,
      station = .config$mode$station,
      level = .config$aggregation$level,
      input_data = .input_data,
      area_code = current_area_code,
      total_cases = nrow(cv_data),
      total_cases_unique = total_cases_unique,
      total_priority_a = priority_a,
      total_priority_b = priority_b,
      total_priority_c = priority_c,
      total_priority_d = priority_d,
      version_app = .config$version$app,
      version_package = .config$version$package,
      version_script = .config$version$script,
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

  if(log_saved) {

    last_row <- DBI::dbGetQuery(.conn, "SELECT last_insert_rowid()")
    current_id <- as.integer(last_row[[1]])

    cv_data <- cv_data |> dplyr::mutate(log_id = current_id)

    DBI::dbWriteTable(
      conn = .conn,
      name = paste0(.input_data, "_cv"),
      value = cv_data,
      append = TRUE
    )
  }

  if(!exists("current_logs_id")) {
    current_logs_id <- NULL
  }
  current_logs_id <- c(current_logs_id, current_id)

  envir <- as.environment(1)
  assign("current_logs_id", current_logs_id, envir = envir)

  return(cv_data)
}

save_logs <- function(.config = getOption("rcbms.config")) {

  if(.config$verbose) {
    cli::cli_h2("Saving Logs")
  }


  if(!exists("current_logs_id")) return(invisible())
  current_logs_id <- unique(current_logs_id)

  conn <- connect_to_rcbms_logs(.config)
  log_tables <- RSQLite::dbListTables(conn)

  logs_input_data <- DBI::dbReadTable(conn, "logs") |>
    dplyr::tibble() |>
    dplyr::filter(id %in% current_logs_id) |>
    dplyr::distinct(input_data) |>
    dplyr::pull(input_data)

  for(i in seq_along(logs_input_data)) {

    input_df <- logs_input_data[i]
    cv_name <- paste0(input_df, "_cv")
    cv_name_current <- paste0(input_df, "_cv_current")

    if(input_df == "hp") {
      by_cv_cols <- c("case_id", "validation_id", "line_number")
    } else if(input_df == "bp") {
      by_cv_cols <- c("uuid", "validation_id")
    }

    cv_logs <- DBI::dbReadTable(conn, cv_name) |>
      dplyr::tibble() |>
      dplyr::mutate(status = NA_character_) |>
      dplyr::filter(log_id %in% current_logs_id)

    cv_logs_current <- cv_logs

    if(cv_name_current %in% log_tables) {

      cv_logs_with_remakrs <- DBI::dbReadTable(conn, cv_name_current) |>
        dplyr::tibble() |>
        dplyr::mutate(id_old = id) |>
        dplyr::filter(!is.na(status))

      if(nrow(cv_logs_with_remakrs) > 0) {
        cv_logs_current <- cv_logs |>
          dplyr::left_join(cv_logs_with_remakrs, by = by_cv_cols, multiple = "first") |>
          dplyr::mutate(id = dplyr::if_else(!is.na(id_old), id_old, id)) |>
          dplyr::select(-id_old)
      }
    }

    RSQLite::dbWriteTable(
      conn = conn,
      name = cv_name_current,
      value = cv_logs_current,
      overwrite = TRUE
    )

  }

  DBI::dbDisconnect(conn)

}



