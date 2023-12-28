#' Title
#'
#' @param .parquet
#' @param .aggregation
#' @param ...
#' @param .config
#' @param .input_data
#' @param .excluded_cases
#' @param .current_area
#' @param .filter_by_area
#'
#' @return
#' @export
#'
#' @examples
#'

get_complete_cases <- function(
  .parquet,
  .aggregation,
  ...,
  .config = getOption('rcbms_config'),
  .input_data = 'hp',
  .excluded_cases = NULL,
  .current_area = NULL,
  .filter_by_area = FALSE
) {

  summary_record <- get_summary_record(.input_data)
  summary_df <- .parquet[[.input_data]][[summary_record]]

  if(is.null(summary_df)) return(NULL)

  result_of_vist <- .config$project[[.input_data]]$variable$result_of_visit
  if(is.null(result_of_vist)) result_of_vist <- "result_of_vist"

  complete_cases_from_rov <- summary_df |>
    dplyr::collect() |>
    dplyr::filter(
      as.integer(hsn) < as.integer(paste(rep(7, 4 + config$project$add_length), collapse = '')),
      as.integer(!!as.name(result_of_vist)) == 1
    ) |>
    create_case_id()

  if(!is.null(.current_area)) {

    areas <- .aggregation$areas_all |>
      dplyr::mutate(
        region_geo = region_code,
        all_area_geo = '',
        all_area_agg = 'All Areas'
      ) |>
      dplyr::select(barangay_geo, dplyr::any_of(paste0(.aggregation$value, '_geo')))

    complete_cases_from_rov <- complete_cases_from_rov |>
      create_barangay_geo() |>
      dplyr::left_join(areas, by = 'barangay_geo') |>
      dplyr::filter(!!as.name(paste0(.aggregation$value, '_geo')) == .current_area) |>
      dplyr::select(-dplyr::any_of('barangay_geo'), -dplyr::ends_with('_geo'))
  }

  complete_cases_from_rov <- complete_cases_from_rov |>
    dplyr::distinct(case_id) |>
    dplyr::pull(case_id)

  roster_record <- get_summary_record(.input_data, 'roster_record')
  roster_df <- .parquet$hp[[roster_record]]

  if(!is.null(roster_record)) {

    complete_cases <- roster_df |>
      dplyr::collect() |>
      create_case_id() |>
      dplyr::filter(case_id %in% complete_cases_from_rov, ...) |>
      dplyr::distinct(case_id) |>
      dplyr::pull(case_id)

  } else {
    complete_cases <- complete_cases_from_rov
  }

  if(!is.null(.excluded_cases)) {
    complete_cases <- complete_cases[!(complete_cases %in% .excluded_cases)]
  }

  return(complete_cases)

}
