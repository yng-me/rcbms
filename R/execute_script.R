#' Execute script
#'
#' @param .config_file Path to config file
#' @param ... Additional arguments passed to \code{\link{load_required_packages}}
#'
#' @return
#' @export
#'
#' @examples
#'

execute_script <- function(.config_file, ...) {

  load_required_packages(...)
  set_config(.config_file)
  update_rcbms()
  load_references()
  read_cbms_data()
  set_aggregation()
  execute_mode()
  save_logs()
  clear_objects()

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

  if(rlang::is_false(.config$execute_mode)) return(invisible())
  envir <- as.environment(1)

  if(.config$verbose) {
    cli::cli_h2("Executing Scripts")
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

  unique_areas <- .aggregation$areas_unique


  for(i in seq_along(.config$input_data)) {

    current_input_data <- .config$input_data[i]

    envir <- as.environment(1)
    assign("current_input_data", current_input_data, envir = envir)

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
        cli::cli_h3(paste0(progress_n, cli::col_br_cyan(area_label)))
        cli::cli_alert_info("Running validation checks")
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

        if(.config$verbose) {
          script_file <- basename(script_files[i]) |>
            stringr::str_remove("\\.(r|R)$")
          if(!grepl("^\\_\\_", script_file)) {
            cli::cli_alert_success(paste0("Processing ", cli::col_br_cyan(script_file), " script file"))
          }
        }
        suppressWarnings(source(script_files[i]))
      }

      generate_output(
        eval(as.name(result_object)),
        .references,
        .aggregation,
        .config = .config
      )
      suppressWarnings(rm(list = "complete_cases"))
    }

  }

}
