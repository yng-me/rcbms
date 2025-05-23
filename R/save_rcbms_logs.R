save_rcbms_logs <- function(
  .data,
  .input_data,
  .references,
  .config,
  .section_ref = NULL,
  .summary_df = NULL
) {

  conn <- connect_to_db_log(.config, .input_data)
  log_tables <- DBI::dbListTables(conn)

  if(!('rcbms_logs' %in% log_tables)) { migrate_db_tables(conn) }

  log_id <- save_current_logs(
    conn,
    .data,
    .input_data,
    log_tables,
    .references,
    .config,
    .section_ref,
    .summary_df
  )

  DBI::dbDisconnect(conn)

  return(log_id)

}

save_current_logs <- function(
  .conn,
  .data,
  .input_data,
  .tables,
  .references,
  .config,
  .section_ref = NULL,
  .summary_df = NULL
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

  summary_stat <- create_summary_stat(.input_data, .config)

  total <- summary_stat$total
  summary_info <- c(summary_info, summary_stat$summary_info)

  if((mode == "tabulation" | mode == "ts") & !is.null(.data)) {

    log_status <- 0

    db_table_name <- "ts"
    by_cv_cols <- "tabulation_id"

    tab_category <- .summary_df$category

    if(!is.null(.summary_df)) {
      summary_info <- .summary_df
    }
    db_data_to_store <- db_data_to_store |>
      dplyr::select(dplyr::any_of(c("id", "tabulation_id", "info")))

  }


  id_int <- DBI::dbReadTable(.conn, 'rcbms_logs') |>
    nrow()

  area_codes <- .config$aggregation$areas  |>
    jsonlite::toJSON() |>
    as.character()

  number_of_ea_processed <- length(.config$aggregation$areas)

  if(tolower(.config$aggregation$areas[1]) == 'all') {
    area_codes <- "[]"
    number_of_ea_processed <- 1
  }

  log_saved <- DBI::dbAppendTable(
    conn = .conn,
    name = "rcbms_logs",
    value = dplyr::tibble(
      log_id = log_id,
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
      source = 1, # primary
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
    )
  )

  if (log_saved) {

    if(log_status == 2) {
      DBI::dbExecute(
        .conn,
        paste0(
          "UPDATE logs SET
            verified_at = CURRENT_TIMESTAMP,
            validated_at = CURRENT_TIMESTAMP
          WHERE log_id = '", log_id, "';"
        )
      )
    }

    if(!is.null(.data)) {

      db_data_to_store <- db_data_to_store |>
        dplyr::mutate(
          log_id = log_id,
          status = 0L
        )

      if(db_table_name %in% .tables) {

        log_id_to_join <- dplyr::tbl(.conn, 'logs') |>
          dplyr::select(id, area_code) |>
          dplyr::collect() |>
          dplyr::filter(grepl(paste0('^', current_area_code), area_code)) |>
          dplyr::pull(id)

        included_status <- 1L

        if(!is.null(.config$include_corrected_status)) {
          if(.config$include_corrected_status) {
            included_status <- 1L
          } else {
            included_status <- 2L
          }
        }

        cv_logs_with_remarks_join <- dplyr::tbl(.conn, db_table_name) |>
          dplyr::filter(
            log_id %in% log_id_to_join,
            status >= included_status,
            status < 9L
          ) |>
          dplyr::select(
            uuid = id,
            last_status = status,
            dplyr::any_of(by_cv_cols)
          ) |>
          dplyr::collect()


        cv_logs_with_remarks_join_uuid <- cv_logs_with_remarks_join$uuid

        remarks_from_ts <- dplyr::tbl(.conn, 'remarks') |>
          dplyr::filter(
            uuid %in% cv_logs_with_remarks_join_uuid,
            status >= included_status,
            status < 9L
          ) |>
          dplyr::select(uuid, created_at, status)

        filter_not_missing <- function(.df) {

          if(mode == 'tabulation' | mode == 'ts') {
            .df |>
              dplyr::filter(!is.na(status))
          } else if (uid %in% names(.df)) {
            .df |>
              dplyr::filter(!is.na(status), !is.na(!!as.name(uid)))
          }
        }

        cv_logs_with_remarks_join <- cv_logs_with_remarks_join |>
          dplyr::full_join(
            remarks_from_ts |>
              dplyr::collect(),
            by = 'uuid',
            relationship = "many-to-many"
          ) |>
          filter_not_missing() |>
          dplyr::arrange(uuid, dplyr::desc(created_at))

        if(nrow(cv_logs_with_remarks_join) > 0) {

          db_data_to_store <- db_data_to_store |>
            dplyr::select(-status) |>
            dplyr::left_join(
              cv_logs_with_remarks_join |>
                dplyr::group_by(dplyr::pick(dplyr::any_of(by_cv_cols))) |>
                tidyr::nest() |>
                dplyr::mutate(
                  data = purrr::map(data, function(x) {
                    x |> head(1) |> dplyr::select(status, old_uuid = uuid)
                  })
                ) |>
                tidyr::unnest(data),
              by = by_cv_cols,
              multiple = "first"
            ) |>
            dplyr::mutate(
              id = dplyr::if_else(is.na(old_uuid), id, old_uuid),
              status = dplyr::if_else(is.na(status), 0L, as.integer(status))
            ) |>
            dplyr::select(-dplyr::any_of("old_uuid"))


          if(.config$db$harmonize_tables & (mode == 'validation' | mode == 'cv')) {

            harmonize_table <- function(.df) {

              if(.input_data != 'bp') {

                .df |>
                  dplyr::add_count(validation_id, case_id, line_number, name = 'm') |>
                  dplyr::add_count(validation_id, case_id, line_number, uuid) |>
                  dplyr::filter(n != m) |>
                  dplyr::group_by(validation_id, case_id, line_number)

              } else {

                .df |>
                  dplyr::add_count(validation_id, barangay_geo, name = 'm') |>
                  dplyr::add_count(validation_id, barangay_geo, uuid) |>
                  dplyr::filter(n != m) |>
                  dplyr::group_by(validation_id, barangay_geo)

              }
            }

            uuid_to_update <- cv_logs_with_remarks_join |>
              dplyr::select(-created_at) |>
              harmonize_table() |>
              tidyr::nest() |>
              dplyr::mutate(
                data = purrr::map(data, function(x) {
                  first_id <- x$uuid[1]
                  x |>
                    dplyr::transmute(
                      uuid_old = uuid,
                      uuid_new = first_id,
                      status,
                      last_status
                    )
                })
              )

            if(nrow(uuid_to_update) > 0) {

              uuid_to_update <- uuid_to_update |>
                tidyr::unnest(data) |>
                dplyr::ungroup() |>
                dplyr::filter(last_status != status, uuid_new != uuid_old)

                for(k in seq_along(uuid_to_update$uuid_old)) {

                  updated_status <- uuid_to_update$status[k]
                  uuid_new <- uuid_to_update$uuid_new[k]
                  uuid_old <- uuid_to_update$uuid_old[k]

                  DBI::dbExecute(
                    .conn,
                    glue::glue("UPDATE {db_table_name} SET id = '{uuid_new}', status = {updated_status} WHERE id = '{uuid_old}';")
                  )

                  DBI::dbExecute(
                    .conn,
                    glue::glue("UPDATE remarks SET id = '{uuid_new}' WHERE id = '{uuid_old}';")
                  )
                }
              }
            }
        }
      }

      DBI::dbAppendTable(
        conn = .conn,
        name = db_table_name,
        value = db_data_to_store
      )
    }
  }

  if (!exists("current_logs_id")) {
    current_logs_id <- NULL
  }
  current_logs_id <- c(current_logs_id, log_id)

  envir <- as.environment(1)
  assign("current_logs_id", current_logs_id, envir = envir)

  return(log_id)
}
