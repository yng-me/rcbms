#' Title
#'
#' @param .dir_to
#' @param .user_id
#' @param .dir_from
#' @param .dir_temp
#' @param .delete_source
#'
#' @return
#' @export
#'
#' @examples
#'

sync_rcbms_logs <- function(
  .dir_to,
  .user_id,
  .dir_from = '//localhost/webdav/RCBMS Logs',
  .dir_temp = 'rclf-temp',
  .delete_source = T
) {

  rclf_files <- list.files(
    .dir_from,
    recursive = T,
    pattern = '\\.rclf',
    full.names = T
  )

  users <- list()

  if(length(rclf_files) == 0) {
    return(users)
  }

  for(i in seq_along(rclf_files)) {

    rclf_i <- rclf_files[i]
    user <- fs::path_ext_remove(basename(rclf_i))

    exdir <- file.path(.dir_to, .dir_temp)
    zip::unzip(rclf_i, exdir = exdir, overwrite = T)

    res <- import_rcbms_logs(
      .dir = .dir_to,
      .user_id = .user_id,
      .dir_to = .dir_temp,
      .delete_source = T
    )

    if(length(res) > 0) {
      users <- c(users, user)
    }

    unlink(exdir, recursive = T, force = T)

  }

  if(.delete_source) {
    unlink(rclf_files, recursive = T, force = T)
  }

  return(unlist(users))

}


