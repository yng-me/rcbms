#' Title
#'
#' @param .parquet
#' @param .config
#' @param ...
#' @param .excluded_cases
#'
#' @return
#' @export
#'
#' @examples

get_complete_cases <- function(
  .parquet,
  ...,
  .config = getOption('rcbms_config'),
  .excluded_cases = NULL,
  .filter_by_area = FALSE
) {

  summary_record <- get_summary_record('hp')
  summary_df <- .parquet$hp[[summary_record]]

  if(is.null(summary_df)) return(NULL)

  complete_cases_from_rov <- summary_df |>
    dplyr::collect() |>
    dplyr::filter(result_of_visit == 1) |>
    create_case_id()

  if(.filter_by_area) {
    complete_cases_from_rov <- complete_cases_from_rov |>
      dplyr::filter(agg_area %in% .config$agg_area)
  }

  complete_cases_from_rov <- complete_cases_from_rov |> dplyr::pull(case_id)

  roster_record <- get_summary_record('hp', 'roster_record')
  roster_df <- .parquet$hp[[roster_record]]

  if(!is.null(roster_record)) {

    complete_cases <- roster_df |>
      dplyr::collect() |>
      create_case_id() |>
      dplyr::filter(
        as.integer(hsn) < 7777,
        case_id %in% complete_cases_from_rov,
        ...
      ) |>
      dplyr::pull(case_id)
  } else {
    complete_cases <- complete_cases_from_rov
  }

  if(!is.null(.excluded_cases)) {
    complete_cases <- complete_cases[!(complete_cases %in% .excluded_cases)]
  }

  return(complete_cases)

}
