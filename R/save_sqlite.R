save_rcbms_logs <- function(.data, .input_data, .references, .config) {

  conn <- connect_to_rcbms_logs(.config)
  log_tables <- DBI::dbListTables(conn)

  save_current_logs(conn, .data, .input_data, log_tables, .references, .config)

  create_remarks_table(conn, log_tables)

  DBI::dbDisconnect(conn)

}


connect_to_rcbms_logs <- function(.config) {
  wd <- create_new_folder(file.path(.config$base, "data", "log"))
  v <- config$version$db
  if (is.null(v)) v <- "0.0.1"
  db_name <- paste0(wd, "/rcbms_logs_v", v, ".db")
  conn <- DBI::dbConnect(RSQLite::SQLite(), db_name)
  return(conn)
}


create_remarks_table <- function(.conn, .tables) {
  if (!("remarks" %in% .tables)) {
    DBI::dbExecute(
      .conn,
      "CREATE TABLE remarks (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        uuid varchar(36),
        user_id varchar(36),
        username varchar(36),
        first_name varchar(36),
        last_name varchar(36),
        role varchar(36),
        status tinyint CHECK (status IN (-1, 0, 1, 2, 3, 4, 5, 9)),
        tag_status tinyint CHECK (tag_status IN (0, 1)),
        remarks text,
        uploaded tinyint CHECK (uploaded IN (0, 1)),
        created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
        updated_at DATETIME DEFAULT NULL
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

  current_area_code <- .config$aggregation$areas[1]

  if (!("logs" %in% .tables)) {
    DBI::dbExecute(
      .conn,
      "CREATE TABLE logs (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        survey_round varchar(4),
        mode varchar(16),
        edit tinyint CHECK (edit IN (0, 1, 2, 3, 4)),
        level tinyint CHECK (level IN (0, 1, 2, 3, 4, 5)),
        stage tinyint CHECK (stage IN (1, 2, 3)),
        station char(5) CHECK (station IN ('CO', 'RO', 'PO', 'LGU')),
        input_data char(3) CHECK (input_data IN ('hp', 'bp', 'ilq')),
        area_code varchar(16),
        total_cases int,
        total_cases_unique int,
        total_priority_a int,
        total_priority_b int,
        total_priority_c int,
        total_priority_d int,
        total int,
        summary text,
        user text,
        user_id varchar(36),
        status tinyint CHECK (status IN (-1, 0, 1, 2, 3, 4, 5, 9)),
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
        created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
        updated_at DATETIME DEFAULT NULL,
        validated_at DATETIME DEFAULT NULL
      );"
    )
  }

  pc <- Sys.info()
  os_release_date <- pc[["version"]] |>
    stringr::str_extract(":\\s\\w*\\s\\w*\\s\\d{2}\\s\\d{2}:\\d{2}:\\d{2}.*;") |>
    stringr::str_remove("^:\\s") |>
    stringr::str_remove(";$")

  uid <- "case_id"
  if (.input_data == "bp") uid <- "barangay_geo"
  current_id <- 1

  if (!is.null(.data)) {
    cv_data <- .data |>
      dplyr::select(dplyr::any_of(c("id", uid, "validation_id", "line_number", "info"))) |>
      dplyr::mutate(log_id = current_id)

    if (uid %in% names(cv_data)) {
      total_cases_unique <- cv_data |>
        dplyr::distinct(!!as.name(uid)) |>
        nrow()
    } else {
      total_cases_unique <- 0
    }


    priority_ref <- .references$validation[[.config$survey_round]][[.input_data]] |>
      dplyr::select(validation_id, dplyr::matches("^priority_level$"))


    if(!("priority_level" %in% names(priority_ref))) {
      priority_ref <- priority_ref |>
        dplyr::mutate(priority_level = NA_character_)
    }

    priority_df <- cv_data |>
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

    log_status <- 0
    total_cases <- nrow(cv_data)
  } else {
    total_cases_unique <- 0
    total_cases <- 0
    total_priority_a <- 0
    total_priority_b <- 0
    total_priority_c <- 0
    total_priority_d <- 0
    log_status <- 2
  }

  total <- NULL
  summary_info <- list()

  if(exists('parquet')) {

    summary_record <- .config$project[[.input_data]]$summary_record

    if(!is.null(summary_record)) {

      final_status_vars <- .config$project[[.input_data]]$final_status$variable

      summary_df <- parquet[[.input_data]][[summary_record]] |>
        dplyr::select(dplyr::any_of(final_status_vars)) |>
        dplyr::collect()

      if(!is.null(summary_df)) {


        if(.input_data == 'hp') {

          # household
          for(k in seq_along(final_status_vars)) {

            final_status_var <- final_status_vars[k]

            summary_info[[final_status_var]] <- summary_df |>
              dplyr::count(!!as.name(final_status_var))
          }

          # roster
          roster_record <- .config$project$hp$roster_record
          sex_var <- .config$project$hp$variable$sex
          age_var <- .config$project$hp$variable$age

          if(!is.null(roster_record) & !is.null(sex_var) & !is.null(age_var)) {

            roster_df <- parquet[[.input_data]][[roster_record]] |>
              dplyr::select(hsn, dplyr::any_of(sex_var), dplyr::contains('_age')) |>
              dplyr::collect() |>
              dplyr::filter(
                as.integer(hsn) < as.integer(
                  paste(rep(7, 4 + .config$project$add_length), collapse = "")
                )
              )

            roster_df_names <- names(roster_df)

            summary_info$sex <- roster_df |>
              dplyr::count(!!as.name(sex_var))

            if(length(grepl('_age_group_five_year$', roster_df_names)) >= 1) {

              age_group_var <- roster_df_names[grepl('_age_group_five_year$', names(roster_df))]

              roster_df <- roster_df |>
                dplyr::select(
                  age = !!as.name(age_group_var[1]),
                  sex = !!as.name(sex_var)
                )

              summary_info$age_group <- roster_df |>
                generate_tab(.cols = c('age', 'sex'), .total_by_cols = T) |>
                factor_col('age') |>
                factor_col('sex') |>
                dplyr::bind_rows(
                  roster_df |>
                    generate_tab(.cols = 'age') |>
                    factor_col('age') |>
                    dplyr::mutate(y = 0, y_fct = 'Both sexes', total_y = total)
                )
            }
          }
        }

        total <- summary_df |> nrow()
      }
    }

  }

  log_saved <- DBI::dbWriteTable(
    conn = .conn,
    name = "logs",
    value = dplyr::tibble(
      survey_round = as.character(.config$survey_round),
      mode = .config$mode$type,
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
    cv_table_name <- paste0(.input_data, "_cv")

    if (.input_data %in% c("hp", "ilq")) {
      by_cv_cols <- c("case_id", "validation_id", "line_number")
    } else if (.input_data == "bp") {
      by_cv_cols <- c("barangay_geo", "validation_id")
    }

    last_row <- DBI::dbGetQuery(.conn, "SELECT last_insert_rowid();")
    current_id <- as.integer(last_row[[1]])

    cv_data <- cv_data |>
      dplyr::mutate(log_id = current_id, status = 0L)

    if (cv_table_name %in% .tables) {
      cv_logs_with_remarks <- DBI::dbReadTable(.conn, cv_table_name) |>
        dplyr::tibble() |>
        dplyr::filter(as.integer(status) != 0) |>
        dplyr::mutate(status = as.integer(status)) |>
        dplyr::mutate(old_uuid = id) |>
        dplyr::select(old_uuid, status, dplyr::any_of(by_cv_cols))

      if (nrow(cv_logs_with_remarks) > 0) {
        cv_data <- cv_data |>
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
      name = cv_table_name,
      value = cv_data,
      append = TRUE
    )
  } else {
    cv_data <- NULL
  }

  if (!exists("current_logs_id")) {
    current_logs_id <- NULL
  }
  current_logs_id <- c(current_logs_id, current_id)

  envir <- as.environment(1)
  assign("current_logs_id", current_logs_id, envir = envir)

  return(cv_data)
}
