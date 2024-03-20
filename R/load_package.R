#' Title
#'
#' @param .pkg
#' @param .install_only
#'
#' @return
#' @export
#'
#' @examples
load_package <- function(.pkg, .install_only = TRUE) {
  if (is_not_installed(.pkg)) {
    utils::install.packages(.pkg)
  }

  library(
    .pkg,
    character.only = T,
    quietly = T,
    warn.conflicts = F,
    verbose = F
  )
}


#' Load required packages
#'
#' @param .load_dependencies
#'
#' @return
#' @export
#'
#' @examples
load_required_packages <- function(.load_dependencies = F) {
  sapply(
    c(
      "arrow",
      "dplyr",
      "tidyr",
      "tibble",
      "stringr",
      "purrr",
      "openxlsx",
      "lubridate",
      "janitor",
      "jsonlite",
      "RSQLite",
      "DBI",
      "gt",
      "yaml",
      "readr",
      "googlesheets4",
      "cli"
    ),
    load_package
  )

  if (.load_dependencies) {
    load_dependencies()
  }
}


load_dependencies <- function() {
  if (is_not_installed("tsg")) {
    if (is_online()) {
      tsg_version <- utils::installed.packages() |>
        tibble::as_tibble() |>
        dplyr::filter(Package == "tsg") |>
        dplyr::pull(Version)

      if (tsg_version != "0.1.3") {
        devtools::install_github("yng-me/tsg")
        cat("\n")
      }
    } else {
      warning("You are currently offline. Check your internet connection.")
    }
  } else {
    if (is_online()) {
      devtools::install_github("yng-me/tsg")
    } else {
      warning("You are currently offline. Check your internet connection and try again.")
    }
  }
}
