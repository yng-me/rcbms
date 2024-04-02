#' Title
#'
#' @param .config
#'
#' @return
#' @export
#'
#' @examples
clear_objects <- function(.config = getOption("rcbms.config")) {
  clear_objects <- .config$clear_objects
  if (is.null(clear_objects)) {
    clear_objects <- TRUE
  }
  if (rlang::is_true(clear_objects) && .config$execute_mode) {
    envir <- as.environment(1)
    obj_env <- ls(envir = envir)
    obj_retain <- c(
      "config",
      "references",
      "aggregation",
      "parquet",
      "cv",
      "ts"
    )

    suppressWarnings(
      rm(
        list = c(
          obj_env[!(obj_env %in% obj_retain)],
          "obj_env",
          "obj_retain"
        ),
        envir = envir
      )
    )
  }
}
