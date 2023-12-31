#' Generate output
#'
#' @param .result
#' @param .refs
#' @param .aggregation
#' @param ...
#' @param .config
#'
#' @return
#' @export
#'
#' @examples
#'

generate_output <- function(
  .result,
  .refs,
  .aggregation,
  ...,
  .config = getOption('rcbms_config')
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

  if(length(.result) == 0) stop('No results found.')

  mode_type <- tolower(.config$mode$type)

  if(mode_type %in% c("validation", "tabulation")) {

    generate_output_fn <- eval(as.name(paste0("generate_", mode_type, "_output")))
    generate_output_fn(.result, ...)

  } else {

    stop('Mode defined in the config is not yet currently supported or maybe mispelled.')

  }
}
