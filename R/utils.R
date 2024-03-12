#' Title
#'
#' @param .dir
#' @param .rm
#'
#' @return
#' @export
#'
#' @examples
#'

create_new_folder <- function(.dir, .rm = FALSE) {

  if(dir.exists(.dir) & .rm) {
    unlink(.dir, recursive = TRUE)
  }

  if(!dir.exists(.dir)){
    dir.create(.dir, recursive = TRUE)
  }
  return(.dir)
}

#' Title
#'
#' @param .site
#'
#' @return
#' @export
#'
#' @examples
#'

is_online <- function(.site = "http://google.com/") {
  tryCatch({
    readLines(.site, n = 1)
    TRUE
  },
  warning = function(w) invokeRestart("muffleWarning"),
  error = function(e) FALSE)
}

#' Title
#'
#' @param path
#'
#' @return
#' @export
#'
#' @examples
#'

clean_path <- function(path) {
  stringr::str_replace_all(
    stringr::str_replace_all(path, '/\\.?/', '/'), '\\/+', '/'
  )
}


#' Title
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'

join_path <- function(...) {
  if(Sys.info()[1] == 'Darwin' | Sys.info()[1] == 'darwin') {
    path <- stringr::str_c(..., collapse = '', sep = '/')
  } else {
    path <- stringr::str_c(..., collapse = '', sep = '\\')
  }

  return(clean_path(path))
}


#' Title
#'
#' @return
#' @export
#'
#' @examples
#'

format_current_date <- function() {
  formatted_date <- paste0(
    stringr::str_pad(lubridate::day(lubridate::today()), width = 2, pad = '0'), ' ',
    lubridate::month(lubridate::today(), label = TRUE, abbr = FALSE), ' ',
    lubridate::year(lubridate::today())
  )
  return(formatted_date)
}


#' Title
#'
#' @param .data
#'
#' @return
#' @export
#'
#' @examples
#'

clean_colnames <- function(.data) {
  str_to_replace <- '\\.|\\-|\\s|\\$\\>|\\<|\\|\\(|\\)|\\[|\\]'
  .data |>
    dplyr::rename_with(~ tolower(stringr::str_replace_all(., str_to_replace, '_'))) |>
    dplyr::rename_with(~ stringr::str_remove(., '^x\\.\\.\\.'))
}


#' Set current working directory relative to the project path
#'
#' @param .base_wd
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'

set_relative_wd <- function(
  ...,
  .base_wd = getOption('rcbms.config')$working_directory
) {

  if(!is.null(.base_wd)) {
    if(!is.null(.base_wd) & typeof(.base_wd) == 'character') {
      wd <- .base_wd
    } else if (!is.null(.base_wd$base)) {
      wd <- .base_wd
    }
  } else {
    wd <- '.'
  }

  join_path(paste0(wd, '/', ...))
}


set_class <- function(.data, .class) {
  class(.data) <- c(.class, class(.data))
  return(.data)
}

is_not_installed <- function(.pkg) {
  rlang::is_false(rlang::is_installed(.pkg))
}

#' Title
#'
#' @param .type
#'
#' @return
#' @export
#'
#' @examples
#'
rcbms_list <- function(.type) {
  new_list <- list()
  set_class(new_list, paste0('rcbms_', .type, '_list'))
}


