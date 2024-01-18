#' Title
#'
#' @param .survey_round
#' @param .values
#' @param .create_file
#'
#' @return
#' @export
#'
#' @examples
#'

generate_config <- function(.survey_round, .directory = NULL) {

  if(is.null(.directory)) {
    .directory <- "./configs/"
  }

  folder <- create_new_folder(.directory)
  global_path <- paste0(folder, "global.yml")
  project_path <- paste0(folder, "project.yml")
  global <- yaml::read_yaml(get_default_config("global"), readLines.warn = FALSE)
  project <- yaml::read_yaml(get_default_config("project"), readLines.warn = FALSE)
  yaml::write_yaml(global, global_path)
  yaml::write_yaml(project, project_path)

  return(global_path)

}


get_default_config <- function(.type) {
  system.file(
    "extdata",
    paste0("config-", .type, ".yml"),
    package = "rcbms"
  )
}
