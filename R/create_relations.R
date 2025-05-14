#' Title
#'
#' @param .config
#'
#' @return
#' @export
#'
#' @examples
#'
create_relations <- function(.config = getOption('rcbms.config')) {

  if(.config$parquet$convert) {

    for(i in seq_along(.config$input_data)) {

      pq <- file.path(.config$base, 'relations', .config$input_data[i])

      relations <- list.files(pq, pattern = '\\.(R|r)$', full.names = T)
      lapply(relations, source)
    }
  }
}
