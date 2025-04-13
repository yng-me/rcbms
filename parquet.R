#' Title
#'
#' @param path
#' @param encryption
#' @param input_data
#' @param pattern
#' @param filters
#' @param metadata
#'
#' @return
#' @export
#'
#' @examples

read_parquet_files <- function(
  path,
  encryption = NULL,
  input_data = NULL,
  pattern = '.*',
  filters = list(),
  metadata = list()
) {

  df <- list()
  pq_files <- list_parquet_files(path, pattern = pattern)
  records <- unique(pq_files$record)

  for(i in seq_along(records)) {

    rec <- records[i]
    cli::cli_alert_info(cli::cli_alert_info('Importing {rec}'))

    pq_file <- dplyr::filter(pq_files, record == rec)
    pq_file <- pq_file$file

    df_temp_pq <- read_parquet_file(pq_file, encryption)

    if('deleted' %in% names(df_temp_pq)) {
      df_temp_pq <- dplyr::filter(df_temp_pq, as.integer(deleted) == 0L)
    }

    if(length(filters) > 0) {

      pq_filters <- names(filters)

      for(j in seq_along(pq_filters)) {

        pq_filter_variable <- pq_filters[j]
        pq_filter_value <- filters[[j]]

        if(
          length(pq_filter_value) > 0 &
          pq_filter_variable %in% names(df_temp_pq)
        ) {

          df_temp_pq <- dplyr::filter(
            df_temp_pq,
            !!as.name(pq_filter_variable) %in% pq_filter_value
          )
        }
      }
    }

    if(!is.null(input_data)) {
      df[[input_data]][[rec]] <- df_temp_pq
    } else {
      df[[rec]] <- df_temp_pq
    }
  }

  if(length(metadata) > 2 & !is.null(metadata$data_dictionary) & !is.na(metadata$valueset)) {
    df <- add_metadata(df, metadata$data_dictionary, metadata$valueset)
  }

  df

}


#' Title
#'
#' @param path
#' @param encryption
#'
#' @return
#' @export
#'
#' @examples
#'

read_parquet_file <- function(path, encryption = NULL) {

  if(is.null(encryption)) {

    arrow::open_dataset(path)

  } else {

    duckdb_conn <- DBI::dbConnect(duckdb::duckdb())
    query_pq <- paste0("SELECT * FROM read_parquet('", path, "', encryption_config = { footer_key: '", encryption$AES_KEY, "' });")

    df <- dDBI::dbGetQuery(duckdb_conn, query_pq) |>
      arrow::arrow_table() |>
      suppressWarnings()

    DBI::dbDisconnect(duckdb_conn, shutdown = TRUE)

    df
  }
}


#' Title
#'
#' @param data
#' @param path
#' @param encryption
#'
#' @return
#' @export
#'
#' @examples
#'

write_parquet_file <- function(data, path, encryption = NULL) {

  if(is.null(encryption)) {

    arrow::write_parquet(data, path)

  } else {

    duckdb_conn <- DBI::dbConnect(duckdb::duckdb())
    DBI::dbWriteTable(duckdb_conn, name = "data", value = data, overwrite = T)
    DBI::dbExecute(duckdb_conn, glue::glue("PRAGMA add_parquet_key('{encryption$AES_KEY}', '{encryption$AES_IV}');"))
    DBI::dbExecute(duckdb_conn, paste0("COPY data TO '", path , "' (ENCRYPTION_CONFIG { footer_key: '", encryption$AES_KEY, "' });"))
    DBI::dbDisconnect(duckdb_conn, shutdown = TRUE)

  }

}


#' Title
#'
#' @param path
#' @param pattern
#' @param as_vector
#'
#' @return
#' @export
#'
#' @examples
#'
#'

list_parquet_files <- function(path, pattern = '.*', as_vector = F) {

  files <- list.files(
    path = path,
    pattern = paste0(pattern, '\\.parquet$'),
    full.names = T,
    recursive = T
  )

  if(as_vector) {
    files
  } else {
    data.frame(
      file = files,
      record = fs::path_ext_remove(basename(files))
    )
  }
}
