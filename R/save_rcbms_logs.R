save_rcbms_logs <- function(.data, .input_data, .references, .config, .section_ref = NULL) {

  conn <- connect_to_rcbms_logs_db(.config, .input_data)
  log_tables <- DBI::dbListTables(conn)

  create_logs_table(conn, log_tables)
  save_current_logs(conn, .data, .input_data, log_tables, .references, .config, .section_ref)

  create_remarks_table(conn, log_tables)

  DBI::dbDisconnect(conn)

}

save_current_logs <- function(
  .conn,
  .data,
  .input_data,
  .tables,
  .references,
  .config,
  .section_ref = NULL
) {


  pc <- Sys.info()
  os_release_date <- pc[["version"]] |>
    stringr::str_extract(":\\s\\w*\\s\\w*\\s\\d{2}\\s\\d{2}:\\d{2}:\\d{2}.*;") |>
    stringr::str_remove("^:\\s") |>
    stringr::str_remove(";$")

  db_data_to_store <- NULL
  mode <- .config$mode$type
  edit <- .config$mode$edit
  uid <- .config$project[[.input_data]]$id
  log_id <- uuid::UUIDgenerate()
  log_status <- 2
  total_cases <- 0
  total_cases_unique <- 0
  total_priority_a <- 0
  total_priority_b <- 0
  total_priority_c <- 0
  total_priority_d <- 0
  partial <- 0
  tab_category <- NULL
  summary_info <- list()
  current_area_code <- NULL

  if(exists('CURRENT_AREA_CODE')) {
    current_area_code <- CURRENT_AREA_CODE
  } else {
    current_area_code <- .config$aggregation$area
  }

  if(!is.null(.data)) {
    db_data_to_store <- .data |>
      add_uuid(.id_name = "id")
  }

  if((mode == "validation" | mode == "cv") & !is.null(.data))  {

    if(is.null(current_area_code)) {
      current_area_code <- get_current_area_code(.data, .input_data, .config)
    }

    db_table_name <- paste0(.input_data, "_cv")
    cv_cols <- c("id", uid, "validation_id", "line_number", "info")

    if(.config$validation$include_additional_info & 'contact' %in% names(.data)) {
      cv_cols <- c(cv_cols, 'contact')
    }

    if (.input_data %in% c("hp", "ilq")) {
      by_cv_cols <- c(uid, "validation_id", "line_number")
    } else if (.input_data %in% c("bp", "shp")) {
      by_cv_cols <- c(uid, "validation_id")
    }

    db_data_to_store <- db_data_to_store |>
      dplyr::select(dplyr::any_of(cv_cols))

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

    if(!is.null(.section_ref)) {

      selected_sec <- .section_ref |>
        dplyr::filter(included, builtin_included) |>
        nrow()

      if(selected_sec < nrow(.section_ref) & edit %in% c(1, 2, 5)) {
        partial <- 1
      }
    }

  }


  if((mode == "tabulation" | mode == "ts") & !is.null(.data)) {

    log_status <- 0

    db_table_name <- "ts"
    by_cv_cols <- "tabulation_id"

    tab_category <- .config$tabulation$category

    db_data_to_store <- db_data_to_store |>
      dplyr::select(dplyr::any_of(c("id", "tabulation_id", "info")))

  }

  summary_stat <- create_summary_stat(.input_data, .config)

  total <- summary_stat$total
  summary_info <- c(summary_info, summary_stat$summary_info)

  id_int <- DBI::dbReadTable(.conn, 'logs') |>
    nrow()

  area_codes <- .config$aggregation$areas  |>
    jsonlite::toJSON() |>
    as.character()

  number_of_ea_processed <- length(.config$aggregation$areas)

  if(tolower(.config$aggregation$areas) == 'all') {
    area_codes <- "[]"
    number_of_ea_processed <- 1
  }

  log_saved <- DBI::dbWriteTable(
    conn = .conn,
    name = "logs",
    value = dplyr::tibble(
      id = log_id,
      id_int = as.integer(id_int + 1),
      survey_round = as.character(.config$survey_round),
      mode = mode,
      edit = edit,
      stage = .config$mode$stage,
      station = toupper(.config$mode$station),
      level = .config$aggregation$level,
      input_data = .input_data,
      category = tab_category,
      area_code = current_area_code,
      area_codes = area_codes,
      number_of_ea_processed = number_of_ea_processed,
      total = total,
      partial = partial,
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
      pc_pid = Sys.getpid(),
      duration = 0
    ),
    append = TRUE
  )

  if (log_saved) {

    if(log_status == 2) {
      DBI::dbExecute(
        .conn,
        paste0(
          "UPDATE logs SET
            verified_at = CURRENT_TIMESTAMP,
            validated_at = CURRENT_TIMESTAMP
          WHERE id = ", log_id, ";"
        )
      )
    }

    if(!is.null(.data)) {

      db_data_to_store <- db_data_to_store |>
        dplyr::mutate(log_id = log_id, status = 0L)

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
  }

  if (!exists("current_logs_id")) {
    current_logs_id <- NULL
  }
  current_logs_id <- c(current_logs_id, log_id)

  envir <- as.environment(1)
  assign("current_logs_id", current_logs_id, envir = envir)

  return(db_data_to_store)
}
