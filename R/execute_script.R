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
          !is.na(!!as.name(sex_variable)) & !!as.name(age_variable) >= 0,
          .input_data = input_df,
          .current_area = unique_area
        )
      } else {
        complete_cases <- NULL
      }

      assign('complete_cases', complete_cases, envir = globalenv())

      script_files <- .refs$script_files |>
        dplyr::filter(input_data == input_df) |>
        dplyr::pull(file)

      lapply(script_files, source)

    }

  }

  return(result)

}
