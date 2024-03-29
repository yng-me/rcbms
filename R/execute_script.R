#' Execute script
#'
#' @param .config_file Path to config file
#' @param ... Additional arguments pass to \code{\link{load_required_packages}}
#' @param .survey_round
#'
#' @return
#' @export
#'
#' @examples
#'

execute_script <- function(.config_file, ..., .survey_round = NULL) {

  with_config <- file.exists(.config_file)
  load_required_packages(...)
  config <- set_config(.config_file)

  if(is.null(.survey_round)) {
    .survey_round <- config$survey_round
  }

  rlang::arg_match(
    .survey_round,
    c("2020", "2021", "2022", "2024"),
    multiple = TRUE
  )

  update_rcbms()
  load_references()

  if(isFALSE(with_config)) return(invisible(NULL))

  envir <- as.environment(1)
  for(i in seq_along(.survey_round)) {
    config <- getOption("rcbms.config")
    config$survey_round <- .survey_round[i]
    options(rcbms.config = config)
    assign("config", config, envir = envir)
    read_cbms_data()
    create_relations()
    set_aggregation()
    execute_mode()
    # save_logs()
    clear_objects()
  }
}


#' Execute script for a given mode
#'
#' @param .parquet parquet data object. The default is \code{get_config("parquet")}.
#' @param .references references object
#' @param .aggregation aggregation object
#' @param .config config object
#'
#' @return
#' @export
#'
#' @examples
#'

execute_mode <- function(
  .parquet = get_config("parquet"),
  .references = get_config("references"),
  .aggregation = get_config("aggregation"),
  .config = getOption("rcbms.config"),
  .excluded_cases = NULL
) {

  envir <- as.environment(1)
  if(rlang::is_false(.config$execute_mode)) {
    if(.config$mode$type == "validation") {
      result_object <- "cv"
    } else {
      result_object <- "ts"
    }
    res_list <- list()
    res_class <- paste0("rcbms_", result_object, "_list")

    assign(
      result_object,
      set_class(res_list, res_class),
      envir = envir
    )
    return(invisible())
  }

  if(.config$verbose) {
    cli::cli_h1("Executing Scripts")
  }

  if(is.null(.references)) stop("References in missing")
  if(is.null(.aggregation)) stop("References in missing")

  if(length(.references$script_files) == 0) {
    if(is.null(.config$verbose)) .config$verbose <- TRUE
    if(.config$verbose) {
      warning(
        cat(
          "SCRIPT WAS NOT EXECUTED:\n| Scripts for",
          crayon::red(crayon::italic(crayon::bold(tolower(.config$mode$type)))),
          "not found.\n| Check your config if the",
          crayon::red(crayon::italic(crayon::bold('mode'))),
          "is defined correctly.\n"
        )
      )
    }
    return(invisible(NULL))
  }

  for(i in seq_along(.config$input_data)) {

    current_input_data <- .config$input_data[i]
    unique_areas <- .aggregation[[current_input_data]]$areas_unique

    if(.config$verbose) {

      if(length(.config$input_data) > 1) {
        progress_n <- paste0("[", i, "/", length(.config$input_data), "]: ")
      } else {
        progress_n <- ""
      }
      cli::cli_h3(
        paste0(progress_n, cli::col_br_cyan(get_input_data_label(current_input_data)))
      )
    }

    envir <- as.environment(1)
    assign("current_input_data", current_input_data, envir = envir)

    if(tolower(.config$mode$type) == "portal") {
      source(paste0(config$base, "/scripts/portal/", current_input_data, "/__initial.R"))
    } else {
      script_files <- .references$script_files |>
        dplyr::filter(input_data == current_input_data) |>
        dplyr::pull(file)

      complete_cases_df <- NULL

      if(current_input_data == "hp") {

        filter_var <- .config$project[[current_input_data]]$variable

        complete_cases_df <- get_complete_cases(
          .parquet,
          .aggregation,
          !is.na(!!as.name(filter_var$age)) &
            !is.na(!!as.name(filter_var$sex)) &
            !!as.name(filter_var$age) >= 0,
          .excluded_cases = .excluded_cases
        )
      }

      for(j in seq_along(unique_areas$code)) {

        if(.config$mode$type == "validation") {
          result_object <- "cv"
        } else {
          result_object <- "ts"
        }

        res_list <- list()
        res_class <- paste0("rcbms_", result_object, "_list")

        assign(
          result_object,
          set_class(res_list, res_class),
          envir = envir
        )

        complete_cases <- NULL
        unique_area <- unique_areas$code[j]

        assign("current_area_code", unique_area, envir = envir)

        area_label <- paste0(unique_area, " ", unique_areas$label[j])

        if(.config$verbose) {
          if(length(unique_areas$code) > 1) {
            progress_n <- paste0("[", j, "/", length(unique_areas$code), "]: ")
          } else {
            progress_n <- ""
          }
          cli::cli_alert_info(
            paste0(progress_n, cli::col_br_cyan(area_label), " ", cli::col_br_cyan("✓"))
          )
        }

        if(!is.null(complete_cases_df)) {

          complete_cases <- complete_cases_df |>
            join_and_filter_area(
              .aggregation,
              .current_area = unique_area
            ) |>
            dplyr::pull(case_id)
        }

        if(!is.null(complete_cases)) {
          assign("complete_cases", complete_cases, envir = envir)
        }

        for(i in seq_along(script_files)) {
          script_file <- basename(script_files[i]) |>
            stringr::str_remove("\\.(r|R)$")

          if(!grepl("^\\_\\_", script_file)) {
            if(.config$verbose) {
              cli::cli_alert_success(
                paste0("Processing ", cli::col_br_yellow(script_file), " script file")
              )
            }

            if(.config$progress) {
              cli::cli_text(paste0("Processing ", script_file, " script file"))
            }
          }
          suppressWarnings(source(script_files[i]))
        }

        if(.config$progress) {
          cli::cli_text('Writing validation output')
        }

        generate_o <- .config[[.config$mode$type]]$generate_output
        if(is.null(generate_o)) generate_o <- FALSE

        if(generate_o) {
          generate_output(
            eval(as.name(result_object)),
            .references,
            .aggregation,
            .config = .config
          )
        }

        suppressWarnings(rm(list = "complete_cases", envir = envir))
      }
    }
  }
}
