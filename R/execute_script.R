#' Execute script
#'
#' @param .config_file
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'

execute_script <- function(.config_file, ...) {

  load_required_packages(...)

  set_config(.config_file)
  compare_version()
  load_references()
  read_cbms_data()
  set_aggregation()
  execute()

}


#' Title
#'
#' @param .parquet
#' @param .references
#' @param .aggregation
#' @param .config
#'
#' @return
#' @export
#'
#' @examples
#'

execute <- function(
  .parquet = get_config("parquet"),
  .references = get_config("references"),
  .aggregation = get_config("aggregation"),
  .config = getOption("rcbms.config"),
  .excluded_cases = NULL
) {

  if(isFALSE(.config$execute_mode)) return(invisible())
  envir <- as.environment(1)

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
          "is correct.\n"
        )
      )
    }
    return(invisible(NULL))
  }


  unique_areas <- .aggregation$areas_unique

  for(i in seq_along(.config$input_data)) {

    input_df <- .config$input_data[i]

    script_files <- .references$script_files |>
      dplyr::filter(input_data == input_df) |>
      dplyr::pull(file)

    complete_cases_df <- NULL

    if(input_df == "hp") {

      filter_var <- .config$project[[input_df]]$variable

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
      cat("Processing: ", unique_area, " ", unique_areas$label[j], "...\n", sep = "")

      if(!is.null(complete_cases_df)) {

        complete_cases <- complete_cases_df |>
          join_and_filter_area(.aggregation, .current_area = unique_area) |>
          dplyr::pull(case_id)

      }

      if(!is.null(complete_cases)) {
        assign("complete_cases", complete_cases, envir = envir)
      }

      lapply(script_files, source)

      generate_output(result, .references, .aggregation)
      suppressWarnings(rm(list = "complete_cases"))
    }

  }

}
