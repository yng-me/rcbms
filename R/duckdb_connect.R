duckdb_connect <- function(.env) {
  conn <- DBI::dbConnect(duckdb::duckdb())
  key_pub <- .env$PQ_KEY_PUB
  key_prv <- .env$PQ_KEY_PRV
  q_encrypt <- paste0("PRAGMA add_parquet_key('", key_pub, "', '", key_prv, "');")
  DBI::dbExecute(conn, q_encrypt)

  return(conn)
}
