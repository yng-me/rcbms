#' Title
#'
#' @param .parquet
#' @param .aggregation
#' @param ...
#' @param .config
#' @param .excluded_cases
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
  .config = getOption("rcbms.config"),
  .excluded_cases = NULL
) {

  summary_record <- get_summary_record("hp")
  summary_df <- .parquet$hp[[summary_record]]

  if(is.null(summary_df)) return(NULL)

  result_of_vist <- .config$project$hp$variable$result_of_visit
  if(is.null(result_of_vist)) result_of_vist <- "result_of_vist"

  df <- summary_df |>
    dplyr::collect() |>
    dplyr::filter(
      as.integer(hsn) < as.integer(paste(rep(7, 4 + config$project$add_length), collapse = '')),
      as.integer(!!as.name(result_of_vist)) == 1
    ) |>
    create_case_id() |>
    dplyr::distinct(case_id, .keep_all = TRUE)

  roster_record <- get_summary_record("hp", "roster_record")
  roster_df <- .parquet$hp[[roster_record]]

  if(!is.null(roster_record)) {

    complete_cases_df <- roster_df |>
      dplyr::collect() |>
      create_case_id() |>
      dplyr::filter(case_id %in% df$case_id, ...) |>
      dplyr::distinct(case_id, .keep_all = TRUE)

  } else {
    complete_cases_df <- df
  }

  if(!is.null(.excluded_cases)) {
    complete_cases_df <- complete_cases_df |>
      dplyr::filter(!(complete_cases %in% .excluded_cases))
  }

  return(
    complete_cases_df |>
      dplyr::select(
        case_id,
        dplyr::starts_with(c("region", "province", "city_mun", "barangay"))
      )
  )

}
