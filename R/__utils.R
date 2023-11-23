create_new_folder <- function(name) {
  if(!dir.exists(name)){
    dir.create(name, recursive = T)
  }
  return(name)
}


clean_path <- function(path) {
  stringr::str_replace_all(path, '/\\.?/', '/')
}


join_path <- function(...) {
  if(Sys.info()[1] == 'Darwin' | Sys.info()[1] == 'darwin') {
    path <- stringr::str_c(..., collapse = '', sep = '/')
  } else {
    path <- stringr::str_c(..., collapse = '', sep = '\\')
  }

  return(clean_path(path))
}


format_date <- function() {
  formatted_date <- paste0(
    stringr::str_pad(lubridate::day(lubridate::today()), width = 2, pad = '0'), ' ',
    lubridate::month(lubridate::today(), label = TRUE, abbr = FALSE), ' ',
    lubridate::year(lubridate::today())
  )
  return(formatted_date)
}


convert_to_na <- function(.data, convert_value = '', pattern = NULL) {

  if(!is.null(pattern)) {
    .data <- .data %>%
      mutate_if(
        is.character,
        ~ str_replace_all(., pattern, convert_value)
      )
  }

  suppressWarnings(
    .data <- .data %>%
      mutate_if(
        is.character,
        ~ if_else(. == convert_value, NA_character_, .)
      )
  )

  return(.data)
}

clean_colnames <- function(.data) {
  str_to_replace <- '\\.|\\-|\\s|\\$\\>|\\<|\\|\\(|\\)|\\[|\\]'
  .data %>%
    dplyr::rename_with(~ tolower(stringr::str_replace_all(., str_to_replace, '_'))) %>%
    dplyr::rename_with(~ stringr::str_remove(., '^x\\.\\.\\.'))
}


