#' Generate output
#'
#' @param .result
#' @param .config
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
generate_output <- function(
  .result,
  .config = getOption('rcbms_config'),
  ...
) {

  if(!exists('refs') & length(.result) == 0) {
    stop('No results and/or validation reference found.')
  }

  mode_type <- tolower(.config$mode$type)

  if(mode_type == 'validation') {

    generate_validation_output(.result, .config, ...)

  } else if (mode_type == 'tabulation') {

    generate_validation_output(.result, .config, ...)

  } else {

    stop('Mode defined in the config is not yet currently supported or maybe mispelled.')

  }
}
