#' Title
#'
#' @param .pkg
#' @param .install_only
#'
#' @return
#' @export
#'
#' @examples
#'

load_package <- function(.pkg, .install_only = TRUE) {

  if(!(.pkg %in% utils::installed.packages()[,'Package'])) {
    utils::install.packages(.pkg)
  }

}


#' Title
#'
#' @return
#' @export
#'
#' @examples
#'

load_dependencies <- function() {

  if(!('crayon' %in% utils::installed.packages()[,'Package'])) {
    if(is_online()) {
      utils::install.packages('crayon')
    } else {
      warning('You are currently offline. Check your internet connection.')
    }
  }

  if('tsg' %in% utils::installed.packages()[,'Package']) {

    if(is_online()) {

      tsg_version <- utils::installed.packages() |>
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
load_required_packages <- function(.load_dependencies = F) {

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


