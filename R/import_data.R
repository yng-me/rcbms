#' Title
#'
#' @param .input_data
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
import_data <- function(.input_data = 'hp', ...) {

  config = getOption('rcbms_config')

  df <- list()
  d <- list_data_files(config)

  for(i in seq_along(d$unique$value)) {
    p <- d$unique$value[i]

    df_src_files <- dplyr::as_tibble(d$all$value) %>%
      dplyr::filter(grepl(paste0(p, '$'), value)) %>%
      dplyr::pull(value)

    df_list <- lapply(df_src_files, function(x) {
      read_cbms_data(x, .input_data = .input_data) |>
        clean_colnames() |>
        harmonize_variable(...)
    })

    p_name <- stringr::str_remove(tolower(p), '\\.(TXT|txt)$')
    df_temp <- do.call('rbind', df_list) |>
      dplyr::tibble() |>
      add_metadata(...)

    pq_folder <- create_new_folder(get_data_path('parquet'))
    pq_path <- file.path(pq_folder, paste0(p_name, '.parquet'))
    arrow::write_parquet(df_temp, pq_path)
    df[[.input_data]][[p_name]] <- arrow::open_dataset(pq_path)

  }

  return(df)
}
