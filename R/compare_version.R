#' Title
#'
#' @return
#' @export
#'
#' @examples
#'

compare_version <- function(error_log_path = NULL) {
  current_version <- NULL

  if(file.exists('rcbms-one/version.json')) {
    v <- jsonlite::read_json("rcbms-one/version.json", simplifyVector = T)
    current_version <- v$version
  } else if (!is.null(error_log_path)) {
    if(file.exists(error_log_path)) {
      unlink(error_log_path, force = T, recursive = T)
    }
  }

  previous_version <- NULL
  if(file.exists('rcbms-one/configs/global.yml')) {
    w <- yaml::read_yaml('rcbms-one/configs/global.yml')
    previous_version <- w$version$app
  }

  if(is.null(current_version) | is.null(previous_version)) {
    return(FALSE)
  }

  return(current_version == previous_version)
}
