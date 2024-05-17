#' Set initial configuration for the project
#'
#' @param .config_dir Path to directory containing valid configuration files. Default config files will be generated if the directory specified does not exist.
#' @param .survey_round Survey round which points to the \code{src} folder of your project directory.
#' @param .input_data Input data which, by default, points to the \code{src} > \code{data} folder of your project directory.
#' @param .include_env Include \code{.env} file. Default is \code{TRUE}. This will be switched to \code{FALSE} if the \code{.env} file is not present. The default location is \code{"./configs/.env"}.
#'
#' @return A list of configuration settings use to execute the script.
#' @export
#'
#' @examples
#'
#' set_config("./configs/global.yml")
#'
set_config <- function(
  .config_dir = "configs",
  .survey_round = NULL,
  .input_data = NULL,
  .include_env = TRUE
) {

  config_base <- getOption("rcbms.config")
  global <- read_config(file.path(.config_dir, "global.yml"))
  project <- read_config(file.path(.config_dir, "project.yml"))

  wd <- global$working_directory
  if(is.null(wd)) wd <- "."
  if(trimws(wd) == "") {
    global$working_directory <- NULL
    wd <- "."
  }

  if(is.null(global$survey_round)) {
    global$survey_round <- "2024"
  }

  global$base <- join_path(wd, "src", global$survey_round)
  global$project <- project[[as.character(global$survey_round)]]

  # ENV
  if(.include_env) {
    env_path <- file.path(wd, .config_dir, ".env")
    if(file.exists(env_path)) {
      global$env <- set_dot_env(env_path)
    }
  }

  config_local <- c(global, config_base)
  config_names <- unique(names(config_local))
  config_final <- list()

  for(i in seq_along(config_names)) {
    config_name <- config_names[i]

    if(inherits(config_local[[config_name]], 'list')) {

      value <- c(global[[config_name]], config_base[[config_name]])

    } else {

      if(!is.null(global[[config_name]])) {
        value <- global[[config_name]]
      } else {
        value <- config_base[[config_name]]
      }
    }

    config_final[[config_name]] <- value

  }

  if(!is.null(.survey_round)) {
    rlang::arg_match(
      .survey_round,
      values = c("2020", "2021", "2022", "2023", "2024"),
      multiple = FALSE
    )
    config_final$survey_round <- .survey_round
  }

  if(!is.null(.input_data)) {
    rlang::arg_match(
      .input_data,
      values = c("hp", "bp", "ilq", "cph", "bs"),
      multiple = TRUE
    )
    config_final$input_data <- .input_data
  }

  if(config_final$mode$type == 'cv') {
    config_final$mode$type <- 'validation'
  } else if(config_final$mode$type == 'ts') {
    config_final$mode$type <- 'tabulation'
  }

  return(invisible(set_class(config_final, "rcbms_config")))

}



#' Read config file
#'
#' @param .config_file
#'
#' @return
#' @export
#'
#' @examples
#'
read_config <- function(.config_file) {

  if(!file.exists(.config_file)) {
    stop("Config file does not exist.")
  }

  ext <- tools::file_ext(.config_file)

  valid_type_ext <- c("yml", "json")
  if(!(ext[1] %in% valid_type_ext)) stop("Invalid file type. Only accepts YAML or JSON file format.")

  if(ext == "yml") {
    config <- yaml::read_yaml(.config_file, readLines.warn = F)
  } else if(ext == "json") {
    config <- jsonlite::fromJSON(.config_file, simplifyVector = T)
  }

  return(config)
}


#' Get config link
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
