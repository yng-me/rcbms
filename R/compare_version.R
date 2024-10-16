#' Title
#'
#' @param current_version
#' @param error_log_path
#'
#' @return
#' @export
#'
#' @examples
#'

compare_version <- function(current_version, error_log_path = NULL) {
  previous_version_1 <- NULL

  if(file.exists('rcbms-one/version.json')) {
    v <- jsonlite::read_json("rcbms-one/version.json", simplifyVector = T)
    previous_version_1 <- v$version
  } else if (!is.null(error_log_path)) {

    if(file.exists(error_log_path)) {
      unlink(error_log_path, force = T, recursive = T)
    }
  }

  previous_version_2 <- NULL
  if(file.exists('rcbms-one/configs/global.yml')) {
    w <- yaml::read_yaml('rcbms-one/configs/global.yml')
    previous_version_2 <- w$version$app
  }

  if(is.null(previous_version_1) | is.null(previous_version_2)) {
    return(FALSE)
  }

  return(previous_version_1 == current_version | previous_version_2 == current_version)
}
