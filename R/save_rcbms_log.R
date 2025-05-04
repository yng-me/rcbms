save_rcbms_log <- function(.data, .config, .metadata) {

  input_data <- .config$input_data[1]
  conn <- connect_to_db_log(.config, input_data)
  migrate_db_tables(conn)

  generated_log_id <- uuid::UUIDgenerate()

  current_area_code <- .metadata$area_code

  if(is.null(current_area_code)) {
    current_area_code <- .config$aggregation$area
  }

  summary_stat <- create_summary_stat(input_data, .config)
  summary_info <- c(
    summary_stat,
    .metadata$summary_info
  )

  area_codes <- .config$aggregation$areas  |>
    jsonlite::toJSON() |>
    as.character()

  number_of_ea_processed <- length(.config$aggregation$areas)

  if(tolower(.config$aggregation$areas[1]) == 'all') {
    area_codes <- "[]"
    number_of_ea_processed <- 1
  }


  key_app <- .metadata$aes$key_app[1]
  iv_app <- .metadata$aes$iv_app[1]
  key_admin <- .metadata$aes$key_admin[1]
  iv_admin <- .metadata$aes$iv_admin[1]

  if(is.null(key_app)) { key_app <- NA_character_ }
  if(is.null(iv_app)) { iv_app <- NA_character_ }
  if(is.null(key_admin)) { key_admin <- NA_character_ }
  if(is.null(iv_admin)) { iv_admin <- NA_character_ }

  log_saved <- DBI::dbAppendTable(
    conn = conn,
    name = "rcbms_logs",
    value = tibble::tibble(
      log_id = generated_log_id,
      survey_round = as.character(.config$survey_round[1]),
      mode = .config$mode$type[1],
      edit = .config$mode$edit[1],
      level = .config$aggregation$level[1],
      stage = .config$mode$stage[1],
      category = .metadata$category,
      station = toupper(.config$mode$station[1]),
      partial = .metadata$partial,
      input_data = input_data,
      area_code = current_area_code,
      area_codes = area_codes,
      number_of_ea_processed = number_of_ea_processed,
      total_cases = .metadata$total_cases,
      total_cases_unique = .metadata$total_cases_unique,
      total_priority_a = .metadata$total_priority_a,
      total_priority_b = .metadata$total_priority_b,
      total_priority_c = .metadata$total_priority_c,
      total_priority_d = .metadata$total_priority_d,
      total = summary_stat$total,
      summary = as.character(jsonlite::toJSON(summary_info)),
      version_app = .config$version$app,
      version_package = .config$version$package,
      version_script = .config$version$script,
      status = .metadata$status,
      key_app = key_app,
      key_admin = key_admin,
      iv_app = iv_app,
      iv_admin = iv_admin,
      pc_os = get_pc_metadata('pc_os'),
      pc_user = get_pc_metadata('pc_user'),
      pc_effective_user = get_pc_metadata('pc_effective_user'),
      pc_os_release_date = get_pc_metadata('pc_os_release_date'),
      pc_os_version = get_pc_metadata('pc_os_version'),
      pc_hardware = get_pc_metadata('pc_hardware'),
      pc_pid = get_pc_metadata('pc_pid'),
      duration = 0,
      verified_at = .metadata$verified_at,
      validated_at = .metadata$validated_at
    )
  )

  cli::cli_text('LOG: {log_saved}')

  if(!is.null(.data) & log_saved) {
    DBI::dbAppendTable(
      conn = conn,
      name = "rcbms_results",
      value = dplyr::mutate(.data, log_id = generated_log_id, .before = 1)
    )
  }

  DBI::dbDisconnect(conn)

  if(!exists("current_logs_id")) {
    current_logs_id <- NULL
  }
  current_logs_id <- c(current_logs_id, generated_log_id)

  envir <- as.environment(1)
  assign("current_logs_id", current_logs_id, envir = envir)

  return(generated_log_id)

}


get_pc_metadata <- function(which) {

  values <- list()

  pc <- Sys.info()

  values$pc_os <- tolower(pc[["sysname"]])
  values$pc_user <- pc[["user"]]
  values$pc_effective_user <- pc[["effective_user"]]
  values$pc_os_release_date <- pc[["version"]] |>
    stringr::str_extract(":\\s\\w*\\s\\w*\\s\\d{2}\\s\\d{2}:\\d{2}:\\d{2}.*;") |>
    stringr::str_remove("^:\\s") |>
    stringr::str_remove(";$")
  values$pc_os_version <- pc[["release"]]
  values$pc_hardware <- pc[["machine"]]
  values$pc_pid <- Sys.getpid()

  values[[which]]

}
