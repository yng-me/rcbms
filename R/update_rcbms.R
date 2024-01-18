update_rcbms <- function(.config = getOption("rcbms.config")) {

  is_package_latest <- utils::compareVersion(
    .config$version$package,
    as.character(utils::packageVersion("rcbms"))
  )

  if(is_package_latest != 0) {

    if(.config$verbose) {
      cli::cli_h1("Checking Versions")
    }

    if(is_online()) {
      devtools::install_github("yng-me/rcbms", upgrade = TRUE)

      wd <- .config$working_directory
      config_updated <- .config
      config_updated$version$package <- as.character(utils::packageVersion("rcbms"))
      config_updated$cbms_round <- NULL
      config_updated$project <- NULL
      config_updated$base <- NULL
      yaml::write_yaml(config_updated, file = paste0(wd, "/configs/global.yml"))

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
