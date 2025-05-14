#' Generate output
#'
#' @param .result
#' @param ...
#' @param .config
#'
#' @return
#' @export
#'
#' @examples
generate_output <- function(
    .result,
    ...,
    .config = getOption("rcbms.config")) {
  if (.config$verbose) {
    cli::cli_alert_info("Saving validation outputs")
  }

  if (is.null(.result)) {
    if (is.null(.config$verbose)) .config$verbose <- TRUE
    if (.config$verbose) {
      warning(
        cat(
          "OUTPUT WAS NOT GENERATED:",
          crayon::red(crayon::italic(crayon::bold("`result`"))),
          "object is",
          crayon::red(crayon::italic(crayon::bold("`NULL`"))),
          "\n"
        )
      )
    }
    return(invisible(NULL))
  }

  if (length(.result) == 0) stop("No results to output.")

  valid_modes <- c("validation", "tabulation")
  mode_types <- tolower(.config$mode$type)
  mode_types <- mode_types[mode_types %in% valid_modes]

  for (i in seq_along(mode_types)) {
    mode_fn <- paste0("generate_", mode_types[i])
    generate_output_fn <- eval(as.name(mode_fn))
    generate_output_fn(.result, .config, ...)
  }
}
