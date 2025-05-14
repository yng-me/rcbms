#' Title
#'
#' @param .record
#' @param .input_data
#' @param .config
#'
#' @return
#' @export
#'
#' @examples
#'
read_parquet_file <- function(.record, .input_data = 'hp', .config = getOption('rcbms.config')) {

  p <- get_data_path('parquet', .input_data, .config)
  pq <- file.path(p, paste0(.record, '.parquet'))

  if(!file.exists(pq)) return(NULL)

  if(.config$use_encryption) {

    duckdb_conn <- DBI::dbConnect(duckdb::duckdb())
    aes_key <- .config$env$AES_KEY
    aes_iv <- .config$env$AES_IV
    q_encrypt <- paste0("PRAGMA add_parquet_key('", aes_key, "', '", aes_iv, "');")
    DBI::dbExecute(duckdb_conn, q_encrypt)

    q_pq <- paste0(
      "SELECT * FROM read_parquet('",
      pq,
      "', encryption_config = { footer_key: '",
      aes_key,
      "' });"
    )

    df <- DBI::dbGetQuery(duckdb_conn, q_pq) |>
      arrow::arrow_table()

    DBI::dbDisconnect(duckdb_conn, shutdown = TRUE)

  } else {
    df <- arrow::open_dataset(pq)
  }

  return(df)
}
