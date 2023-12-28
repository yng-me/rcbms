#' Title
#'
#' @param .config_file
#' @param .include_env
#' @param .save_as_options
#'
#' @return
#' @export
#'
#' @examples
#'
set_config <- function(.config_file, .include_env = TRUE, .save_as_options = TRUE) {

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
  version_dir <- join_path(paste0(wd, '/version.json'))
  if(file.exists(version_dir)) {
    current_version <- jsonlite::read_json(version_dir)
  }

  config$version <- current_version
  config$base <- join_path(wd, 'src', config$cbms_round)

  rel_wd <- stringr::str_remove(.config_file, '(config|global)\\.(YML|yml|JSON|json)$')

  project <- NULL
  if(file.exists(join_path(rel_wd, 'project.yml'))) {
    project <- yaml::read_yaml(join_path(rel_wd, 'project.yml'), readLines.warn = F)
  } else if (file.exists(join_path(rel_wd, 'project.json'))) {
    project <- jsonlite::fromJSON(join_path(rel_wd, 'project.json'), simplifyVector = T)
  }

  config$project <- project[[as.character(config$cbms_round)]]

  if(is.null(config$mode$output$generate)) {
    config$mode$output$generate <- FALSE
  }

  # ENV
  if(.include_env) {
    config$env <- set_dot_env(join_path(rel_wd, '.env'))
  }

  if(.save_as_options) {
    options(rcbms_config = config)
  }

  return(config)
}
