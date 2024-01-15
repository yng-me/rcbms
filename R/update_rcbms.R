update_rcbms <- function(.config = getOption("rcbms.config")) {

  is_package_latest <- utils::compareVersion(
    .config$version$package,
    as.character(utils::packageVersion("rcbms"))
  )

  if(is_package_latest != 0) {

    if(.config$verbose) {
      cli::cli_h2("Checking Versions")
    }

    if(is_online()) {
      devtools::install_github("yng-me/rcbms", upgrade = TRUE)

      print(as.character(utils::packageVersion("rcbms")))

      .config$version$package <- as.character(utils::packageVersion("rcbms"))
      wd <- .config$working_directory
      yaml::write_yaml(.config, file = paste0(wd, "/configs/global.yml"))

    } else {
      cli::cli_alert_warning(paste0(
        cli::col_red(
          cli::style_bold("rcbms"),
          " package in outdated. ",
          "Connect to the Internet to download the latest version."
        )
      ))
    }
  }
}
