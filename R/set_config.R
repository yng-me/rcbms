#' Set initial configuration for the project
#'
#' @param .config_file Path to config file. Default is \code{"./configs/global.yml"}. If the file is not present, a default config file will be generated.
#' @param .include_env Include \code{.env} file. Default is \code{TRUE}. This will be switched to \code{FALSE} if the \code{.env} file is not present. The default location is \code{"./configs/.env"}.
#' @param .save_as_options Save as options in the global environment. Default is \code{TRUE}.
#' @param .assign_name Name to assign to the config object. Default is \code{config}.
#'
#' @return A list of configuration settings use to execute the script.
#' @export
#'
#' @examples
#'
#' set_config("./configs/global.yml")
#'
set_config <- function(
    .config_file,
    .include_env = TRUE,
    .save_as_options = TRUE,
    .assign_name = "config") {
  valid_type_ext <- c("yml", "json")
  ext <- tools::file_ext(.config_file)

  if (!(ext[1] %in% valid_type_ext)) stop("Invalid file type.")

  if (!file.exists(.config_file)) {
    ext <- "yml"
    .config_file <- generate_config(.survey_round = 2024)
    .include_env <- FALSE
  }

  if (ext == "yml") {
    config <- yaml::read_yaml(.config_file, readLines.warn = F)
  }

  if (ext == "json") {
    config <- jsonlite::fromJSON(.config_file, simplifyVector = T)
  }

  # VERSION
  wd <- config$working_directory
  if (is.null(wd)) wd <- "."
  if (trimws(wd) == "") {
    config$working_directory <- NULL
    wd <- "."
  }

  if (!is.null(config$cbms_round)) {
    cli::cli_alert_warning(
      paste0(
        "Global config has been renamed from ",
        cli::style_bold(cli::col_black("cbms_round")), " to",
        cli::style_bold(cli::col_black("survey_round"))
      )
    )
    config$survey_round <- config$cbms_round
  }

  config$base <- join_path(wd, "src", config$survey_round)

  rel_wd <- stringr::str_remove(.config_file, "(config|global)\\.(YML|yml|JSON|json)$")

  project <- NULL
  if (file.exists(join_path(rel_wd, "project.yml"))) {
    project <- yaml::read_yaml(join_path(rel_wd, "project.yml"), readLines.warn = F)
  } else if (file.exists(join_path(rel_wd, "project.json"))) {
    project <- jsonlite::fromJSON(join_path(rel_wd, "project.json"), simplifyVector = T)
  }

  config$project <- project[[as.character(config$survey_round)]]

  if (is.null(config$progress)) {
    config$progress <- FALSE
  }


  # ENV
  if (.include_env) {
    env_path <- join_path(rel_wd, ".env")
    if (file.exists(env_path)) {
      config$env <- set_dot_env(env_path)
    }
  }

  config <- set_class(config, "rcbms_config")

  if (.save_as_options) {
    options(rcbms.config = config)
  }

  envir <- as.environment(1)
  if (length(config$input_data) == 1) {
    assign("current_input_data", config$input_data[1], envir = envir)
  }

  if (!is.null(.assign_name)) {
    assign(.assign_name, config, envir = envir)
  }

  return(invisible(config))
}


#' Title
#'
#' @param .key
#'
#' @return
#' @export
#'
#' @examples
get_config <- function(.key) {
  config <- getOption("rcbms.config")
  obj <- config$links[[.key]]

  if (!is.null(obj)) {
    if (exists(obj)) {
      eval(as.name(obj))
    } else {
      return(NULL)
    }
  } else {
    return(NULL)
  }
}
