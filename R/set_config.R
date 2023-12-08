#' Title
#'
#' @param .config_file
#' @param .cwd
#'
#' @return
#' @export
#'
#' @examples
set_config <- function(.config_file) {

  valid_type_ext <- c('yml', 'json')
  ext <- tools::file_ext(.config_file)

  if(!(ext[1] %in% valid_type_ext)) {
    stop("Accepts valid file type only for the config. Use either '.yml' or '.json'.")
  }

  if(ext == 'yml') {
    config <- yaml::read_yaml(.config_file, readLines.warn = F)
  }

  if(ext == 'json') {
    config <- jsonlite::fromJSON(.config_file, simplifyVector = T)
  }

  # VERSION
  wd <- config$working_directory
  current_version <- NULL
  if(is.null(wd)) wd <- './'
  version_dir <- join_path(paste0(wd, '/.version.json'))
  if(file.exists(version_dir)) {
    current_version <- jsonlite::read_json(version_dir)
  }

  config$version <- current_version

  # ENV
  wd <- stringr::str_remove(.config_file, 'config\\.(YML|yml|JSON|json)$')

  config$env <- set_dot_env(join_path(wd, '.env'))

  options(rcbms_config = config)

  return(config)
}
