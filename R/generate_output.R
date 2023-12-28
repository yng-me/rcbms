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
  .refs,
  .config = getOption('rcbms_config'),
  ...
) {

  if(is.null(.result)) {
    if(is.null(.config$verbose)) .config$verbose <- TRUE
    if(.config$verbose) {
      warning(
        cat(
          'OUTPUT WAS NOT GENERATED:',
          crayon::red(crayon::italic(crayon::bold('`result`'))),
          'object is',
          crayon::red(crayon::italic(crayon::bold('`NULL`'))),
          '\n'
        )
      )
    }
    return(invisible(NULL))
  }
  if(!config$mode$output$generate) return(invisible(NULL))

  if(length(.result) == 0) {
    stop('No results found.')
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
