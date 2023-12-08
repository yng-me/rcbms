#' Title
#'
#' @param .dictionary
#' @param .valueset
#' @param .input_data
#'
#' @return
#' @export
#'
#' @examples
import_data <- function(
  .input_data = getOption('rcbms_config')$input_data,
  .dictionary = NULL,
  .valueset = NULL
) {

  config = getOption('rcbms_config')
  .input_data <- check_input_data(config$input_data)

  read_from_parquet <- FALSE
  if(!is.null(config$read_from_parquet)) {
    read_from_parquet <- config$read_from_parquet
  }

  df <- list()

  for(i in seq_along(.input_data)) {

    df_input <- .input_data[i]
    df_files <- list_data_files(config, df_input)

    for(j in seq_along(df_files$unique$value)) {

      p <- df_files$unique$value[j]
      file_format <- get_file_format(config, df_input)
      p_name <- stringr::str_remove(tolower(p), file_format)
      pq_folder <- create_new_folder(get_data_path('parquet', df_input))
      pq_path <- file.path(pq_folder, paste0(p_name, '.parquet'))

      # cat('Importing ', p_name, '...\n', sep = '')

      if(!read_from_parquet) {

        df_src_files <- dplyr::as_tibble(df_files$all$value) |>
          dplyr::filter(grepl(paste0(p, '$'), value)) |>
          dplyr::pull(value)

        df_list <- lapply(df_src_files, function(x) {
          suppressWarnings(
            read_cbms_data(x, .input_data = df_input) |>
              clean_colnames() |>
              harmonize_variable(.dictionary)
          )
        })

        df_temp <- do.call('rbind', df_list) |>
          dplyr::tibble() |>
          add_metadata(.dictionary, .valueset)

        arrow::write_parquet(df_temp, pq_path)

      }

      df[[df_input]][[p_name]] <- arrow::open_dataset(pq_path)

    }

  }

  return(df)
}

