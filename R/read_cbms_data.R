#' Title
#'
#' @param .references
#' @param .config
#' @param .assign_name
#'
#' @return
#' @export
#'
#' @examples
#'

read_cbms_data <- function(
  .references = get_config("references"),
  .config = getOption('rcbms_config'),
  .assign_name = "parquet"
) {

  .input_data <- check_input_data(.config$input_data)
  mode <- tolower(.config$mode$type)

  read_from_parquet <- FALSE
  if(!is.null(.config$read_from_parquet)) {
    read_from_parquet <- .config$read_from_parquet
  }

  df <- list()

  for(i in seq_along(.input_data)) {

    df_input <- .input_data[i]
    df_files <- list_data_files(.config, df_input)

    for(j in seq_along(df_files$unique$value)) {

      p <- df_files$unique$value[j]
      file_format <- get_file_format(.config, df_input)
      p_name <- stringr::str_remove(tolower(p), file_format)
      pq_folder <- create_new_folder(get_data_path('parquet', df_input))
      pq_path <- file.path(pq_folder, paste0(p_name, '.parquet'))

      print(p_name)

      if(!read_from_parquet) {

        df_src_files <- dplyr::as_tibble(df_files$all$value) |>
          dplyr::filter(grepl(paste0(p, '$'), value)) |>
          dplyr::pull(value)

        df_list <- lapply(df_src_files, function(x) {
          suppressWarnings(
            import_data(x, .input_data = df_input) |>
              clean_colnames() |>
              harmonize_variable(.references$data_dictionary)
          )
        })

        df_temp <- do.call('rbind', df_list) |> dplyr::tibble()

        assign("df_temp", df_temp, envir = globalenv())

        if(mode == 'portal' | mode == 'tabulation') {
          src_file <- join_path(.config$base, 'tidy', .input_data, paste0(p_name, '.R'))
          if(file.exists(src_file)) source(src_file)
        }

        df_temp <- df_temp |>
          add_metadata(.references$data_dictionary, .references$valueset)

        arrow::write_parquet(df_temp, pq_path)

      }

      df[[df_input]][[p_name]] <- arrow::open_dataset(pq_path)

    }

  }

  suppressWarnings(rm(list = 'df_temp', envir = globalenv()))

  if(!is.null(.assign_name)) {

    .config$links$parquet <- .assign_name
    options(rcbms_config = .config)

    assign(.assign_name, df, envir = globalenv())
  }

  return(df)
}


tidy_data_frame <- function(.data, .base, .input_data, .record, ...) {

  src_file <- join_path(.base, 'tidy', .input_data, paste0(.record, '.R'))

  if(!file.exists(src_file)) return(.data)
  source(src_file)

  if(!exists('tidy_cbms_data_temp')) return(.data)

  .data |> tidy_cbms_data_temp(...)

}


#' Title
#'
#' @param .parquet
#' @param .record
#' @param .input_data
#' @param ...
#' @param .config
#' @param .complete_cases
#'
#' @return
#' @export
#'
#' @examples
#'

tidy_cbms_data <- function(
  .parquet,
  .record,
  .input_data,
  ...,
  .config = getOption('rcbms_config'),
  .complete_cases = NULL
) {

  if(exists('tidy_cbms_data_temp')) {
    suppressWarnings(rm(list = 'tidy_cbms_data_temp'))
  }

  df <- .parquet[[.record]] |>
    dplyr::collect() |>
    create_case_id()

  if(!is.null(.complete_cases)) {
    df <- df |> dplyr::filter(case_id %in% .complete_cases)
  }

  df |> tidy_data_frame(.config$base, .record, .input_data, ...)

}

