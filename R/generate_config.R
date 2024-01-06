#' Title
#'
#' @param .cbms_round
#' @param .values
#' @param .create_file
#'
#' @return
#' @export
#'
#' @examples
#'

generate_config <- function(.cbms_round, .directory = NULL) {

  global_config_file <- system.file("extdata", "global.yml", package = "rcbms")
  project_config_file <- system.file("extdata", "project.yml", package = "rcbms")

  if(is.null(.directory)) {
    .directory <- "./configs/"
  }

  folder <- create_new_folder(.directory)
  global_path <- paste0(folder, "global.yml")
  project_path <- paste0(folder, "project.yml")
  global <- yaml::read_yaml(global_config_file, readLines.warn = FALSE)
  project <- yaml::read_yaml(project_config_file, readLines.warn = FALSE)
  yaml::write_yaml(global, global_path)
  yaml::write_yaml(project, project_path)

  return(global_path)

}
