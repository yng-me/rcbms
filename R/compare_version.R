compare_version <- function(.config = getOption("rcbms_config")) {

  is_package_latest <- utils::compareVersion(
    .config$version$package,
    as.character(utils::packageVersion("rcbms"))
  )
  # if(is_package_latest == 1) update.packages("rcbms")

}
