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

  if(!install_only) {

    if(!('crayon' %in% installed.packages()[,'Package'])) install.packages('crayon')
    library(crayon)
  }

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

  if('tsg' %in% installed.packages()[,'Package']) {

    tsg_version <- installed.packages() |>
      tibble::as_tibble() |>
      dplyr::filter(Package == 'tsg') |>
      dplyr::pull(Version)

    if(tsg_version != '0.1.3') {
      devtools::install_github('yng-me/tsg')
      cat('\n')
    }

  } else {
    devtools::install_github('yng-me/tsg')
    cat('\n')
  }

  library(tsg)
}


