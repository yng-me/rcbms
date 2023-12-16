#' Title
#'
#' @param pkg
#' @param install_only
#'
#' @return
#' @export
#'
#' @examples
load_package <- function(pkg, install_only = TRUE) {

  if(!(pkg %in% installed.packages()[,'Package'])) {
    install.packages(pkg)
  }

  library(
    pkg,
    character.only = T,
    quietly = T,
    warn.conflicts = F,
    verbose = F
  )

}


#' Title
#'
#' @return
#' @export
#'
#' @examples
load_dependencies <- function() {

  if(!('crayon' %in% installed.packages()[,'Package'])) {
    if(is_online()) {
      install.packages('crayon')
    } else {
      warning('You are currently offline. Check your internet connection.')
    }
  }

  if('tsg' %in% installed.packages()[,'Package']) {

    if(is_online()) {

      tsg_version <- installed.packages() |>
        tibble::as_tibble() |>
        dplyr::filter(Package == 'tsg') |>
        dplyr::pull(Version)

      if(tsg_version != '0.1.3') {
        devtools::install_github('yng-me/tsg')
        cat('\n')
      }

    } else {
      warning('You are currently offline. Check your internet connection.')
    }

  } else {

    if(is_online()) {
      devtools::install_github('yng-me/tsg')
    } else {
      warning('You are currently offline. Check your internet connection and try again.')
    }
  }

  library(crayon)
  library(tsg)
}


#' Load required packages
#'
#' @param .load_dependencies
#'
#' @return
#' @export
#'
#' @examples
#'
load_required_packages <- function(.load_dependencies = T) {

  sapply(
    c(
      'arrow',
      'dplyr',
      'tidyr',
      'tibble',
      'stringr',
      'purrr',
      'openxlsx',
      'lubridate',
      'janitor',
      'jsonlite',
      'yaml',
      'readr',
      'quarto',
      'devtools',
      'googlesheets4'
    ),
    load_package
  )

  if(.load_dependencies) {
    load_dependencies()
  }
}


