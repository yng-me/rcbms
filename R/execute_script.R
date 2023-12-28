#' Title
#'
#' @param .parquet
#' @param .refs
#' @param .aggregation
#' @param .config
#' @param .filter_complete_cases
#'
#' @return
#' @export
#'
#' @examples
#'

execute_script <- function(
  .parquet,
  .refs,
  .aggregation,
  .config = getOption('rcbms_config'),
  .filter_complete_cases = T
) {

  assign('result', list(), envir = globalenv())

  if(length(.refs$script_files) == 0) {
    if(is.null(.config$verbose)) .config$verbose <- TRUE
    if(.config$verbose) {
      warning(
        cat(
          'SCRIPT WAS NOT EXECUTED:\n| Scripts for',
          crayon::red(crayon::italic(crayon::bold(tolower(.config$mode$type)))),
          'not found.\n| Check your config if the',
          crayon::red(crayon::italic(crayon::bold('mode'))),
          'is correct.\n'
        )
      )
    }
    return(invisible(NULL))
  }

  script_files <- .refs$script_files |>
    dplyr::filter(input_data == input_df) |>
    dplyr::pull(file)

  unique_areas <- .aggregation$areas_unique

  for(i in seq_along(unique_areas$code)) {

    unique_area <- unique_areas$code[i]
    cat('Processing: ', unique_area, ' ', unique_areas$label[i], '...\n', sep = '')

    for(j in seq_along(.config$input_data)) {

      input_df <- .config$input_data[j]

      if(input_df == 'hp') {

        age_variable <- .config$project[[input_df]]$variable$age
        sex_variable <- .config$project[[input_df]]$variable$sex

        complete_cases <- get_complete_cases(
          .parquet,
          .aggregation,
          !is.na(!!as.name(sex_variable)),
          !is.na(!!as.name(age_variable)),
          !!as.name(age_variable) >= 0,
          .input_data = input_df,
          .current_area = unique_area
        )
      } else {
        complete_cases <- NULL
      }

      assign('complete_cases', complete_cases, envir = globalenv())

      lapply(script_files, source)

    }

  }

  return(result)

}
