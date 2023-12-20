#' Title
#'
#' @param .parquet
#' @param .refs
#' @param .config
#'
#' @return
#' @export
#'
#' @examples
#'

execute_script <- function(
  .parquet,
  .refs,
  .config = getOption('rcbms_config'),
  .filter_complete_cases = T
) {

  # complete_cases <- get_complete_cases(.parquet)

  script_files <- refs$script_files |>
    dplyr::filter(input_data == 'hp') |>
    dplyr::pull(file)

  lapply(script_files, source)

}
