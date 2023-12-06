#' Read Environment Variable
#'
#' @param .path
#'
#' @return List of environment variables defined
#' @export
#'
#' @examples
#' read_dot_env()
#'

read_dot_env <- function(.path = '.env') {

  if(!file.exists(.path)) {
    stop('.env file does not exist.')
  }

  env_file <- readr::read_file(.path)
  env_values <- unlist(stringr::str_split(env_file, '\n')[[1]])
  env_values <- env_values[env_values != '']

  if(length(env_values) == 0) {
    stop('.env file is empty.')
  }

  env <- list()
  for(i in seq_along(env_values)) {

    env_value <- unlist(stringr::str_split(env_values[i], '=')[[1]])
    key <- stringr::str_trim(env_value[1])
    value <- stringr::str_trim(env_value[2])

    env[[key]] <- stringr::str_remove_all(value, '\\"')
  }

  return(env)
}


get_env <- function(.key) {
  .key <- as.character(substitute(.key))
  env <- read_dot_env()
  if(!(.key %in% names(env))) {
    stop(paste0(.key, ' is not defined in the .env.'))
  }
  return(env[[.key]])
}
