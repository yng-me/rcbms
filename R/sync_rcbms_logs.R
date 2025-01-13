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
  .dir_from,
  .dir_temp = 'rclf-temp',
  .delete_source = T
) {

  rclf_files <- list.files(
      .dir_from,
      recursive = T,
      pattern = '\\.rclf',
      full.names = T
    ) |>
    dplyr::as_tibble() |>
    dplyr::filter(!grepl(.user_id, value)) |>
    dplyr::pull(value)

  files_sync <- list()

  if(length(rclf_files) == 0) {
    return(files_sync)
  }

  dir_db_from <- file.path(.dir_to, 'db', .user_id)
  backup_dir <- create_new_folder(file.path(.dir_to, 'backup'))

  file.copy(
    from = dir_db_from,
    to = backup_dir,
    overwrite = T,
    recursive = T
  )

  for(i in seq_along(rclf_files)) {

    rclf_i <- rclf_files[i]
    file_sync <- fs::path_ext_remove(basename(rclf_i))
    filename <- basename(rclf_i) |>
      stringr::str_remove_all('\\.rclf$')

    exdir <- file.path(.dir_to, .dir_temp)
    zip::unzip(rclf_i, exdir = exdir, overwrite = T)

    files_sync[[filename]] <- import_rcbms_logs(
      .dir = .dir_to,
      .user_id = .user_id,
      .dir_to = .dir_temp,
      .delete_source = T
    )

    unlink(exdir, recursive = T, force = T)

  }

  if(.delete_source) {
    unlink(rclf_files, recursive = T, force = T)
  }

  return(files_sync)

}
