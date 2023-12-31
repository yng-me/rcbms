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

execute_script <- function(
  .parquet,
  .references,
  .aggregation,
  .config = getOption("rcbms_config"),
  .excluded_cases = NULL
) {

  if(.config$mode$type == "validation") {
    result_object <- "cv"
  } else {
    result_object <- "ts"
  }
  assign(result_object, list(), envir = globalenv())

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

  script_files <- .references$script_files |>
    dplyr::filter(input_data == input_df) |>
    dplyr::pull(file)

  unique_areas <- .aggregation$areas_unique

  for(i in seq_along(.config$input_data)) {

    input_df <- .config$input_data[i]
    complete_cases_df <- NULL

    if(input_df == "hp") {

      filter_var <- .config$project[[input_df]]$variable

      complete_cases_df <- get_complete_cases(
        .parquet,
        .aggregation,
        !is.na(!!as.name(filter_var$age)) &
          !is.na(!!as.name(filter_var$sex)) &
          !!as.name(age_variable) >= 0,
        .excluded_cases = .excluded_cases
      )
    }

    for(j in seq_along(unique_areas$code)) {

      if(.config$mode$type == "validation") {
        result_object <- "cv"
      } else {
        result_object <- "ts"
      }
      assign(result_object, list(), envir = globalenv())

      complete_cases <- NULL
      unique_area <- unique_areas$code[j]
      cat("Processing: ", unique_area, " ", unique_areas$label[j], "...\n", sep = "")

      if(!is.null(complete_cases_df)) {

        complete_cases <- complete_cases_df |>
          join_and_filter_area(.aggregation, .current_area = unique_area) |>
          pull(case_id)

      }

      if(!is.null(complete_cases)) {
        assign("complete_cases", complete_cases, envir = globalenv())
      }

      lapply(script_files, source)

      generate_output(result, .references, .aggregation)
      suppressWarnings(rm(list = "complete_cases"))
    }

  }

}
