#' Title
#'
#' @param name
#'
#' @return
#' @export
#'
#' @examples
create_new_folder <- function(name) {
  if(!dir.exists(name)){
    dir.create(name, recursive = T)
  }
  return(name)
}


#' Title
#'
#' @param path
#'
#' @return
#' @export
#'
#' @examples
clean_path <- function(path) {
  stringr::str_replace_all(path, '/\\.?/', '/')
}


#' Title
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
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
clean_colnames <- function(.data) {
  str_to_replace <- '\\.|\\-|\\s|\\$\\>|\\<|\\|\\(|\\)|\\[|\\]'
  .data |>
    dplyr::rename_with(~ tolower(stringr::str_replace_all(., str_to_replace, '_'))) |>
    dplyr::rename_with(~ stringr::str_remove(., '^x\\.\\.\\.'))
}
