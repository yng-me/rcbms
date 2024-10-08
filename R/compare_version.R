#' Title
#'
#' @return
#' @export
#'
#' @examples
#'

compare_version <- function() {
  current_version <- NULL

  if(file.exists('version.json')) {
    v <- jsonlite::read_json("version.json", simplifyVector = T)
    current_version <- v$version
  }

  previous_version <- NULL
  if(file.exists('configs/global.yml')) {
    w <- yaml::read_yaml('configs/global.yml')
    previous_version <- w$version$app
  }

  if(is.null(current_version) | is.null(previous_version)) {
    return(FALSE)
  }

  return(current_version == previous_version)
}
