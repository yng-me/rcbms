#' Title
#'
#' @param config_file
#' @param cwd
#'
#' @return
#' @export
#'
#' @examples
set_config <- function(.config_file, .cwd = NULL) {

  valid_type_ext <- c('yml', 'json')
  ext <- tools::file_ext(.config_file)

  if(!(ext[1] %in% valid_type_ext)) {
    stop("Accepts valid file type only for the config. Use either '.yml' or '.json'.")
  }

  if(is.null(.cwd)) {
    file <- clean_path(.config_file)
  } else {
    file <- clean_path(paste0(.cwd, '/', .config_file))
  }

  if(ext == 'yml') {
    config <- yaml::read_yaml(file, readLines.warn = F)
  }

  if(ext == 'json') {
    config <- jsonlite::fromJSON(file, simplifyVector = T)
  }

  return(config)
}
