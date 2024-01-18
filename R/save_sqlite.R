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

  if(uid %in% names(cv_data)) {
    total_cases_unique <- cv_data |>
      dplyr::distinct(!!as.name(uid)) |>
      nrow()
  } else {
    total_cases_unique <- 0
  }

  cv_ref <- .references$validation |>
    dplyr::collect() |>
    dplyr::filter(
      survey_round == as.integer(.config$survey_round),
      input_data == .input_data
    ) |>
    dplyr::select(validation_id, priority_level)

  priority_df <- cv_data |>
    dplyr::select(validation_id) |>
    dplyr::left_join(
      .references$validation |>
        dplyr::collect() |>
        dplyr::filter(
          survey_round == as.integer(.config$survey_round),
          input_data == .input_data
        ) |>
        dplyr::select(validation_id, priority_level),
      by = "validation_id"
    ) |>
    dplyr::mutate(priority_level = stringr::str_trim(tolower(priority_level)))

  get_priority <- function(.level) {
    priority_df |>
      dplyr::filter(priority_level == .level) |>
      nrow()
  }

  log_saved <- DBI::dbWriteTable(
    conn = .conn,
    name = "logs",
    value = dplyr::tibble(
      round = .config$survey_round,
      mode = .config$mode$type,
      edit = .config$mode$edit,
      source = .config$mode$source,
      station = toupper(.config$mode$station),
      level = .config$aggregation$level,
      input_data = .input_data,
      area_code = current_area_code,
      total_cases = nrow(cv_data),
      total_cases_unique = total_cases_unique,
      total_priority_a = get_priority('a'),
      total_priority_b = get_priority('b'),
      total_priority_c = get_priority('c'),
      total_priority_d = get_priority('d'),
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
