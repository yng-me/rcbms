save_bp_data <- function(.data, .path) {

  df <- list()

  arrow::write_parquet(
    df_temp_dim$bpq_data,
    paste0(.path, '/bpq_data.parquet')
  )

  arrow::write_parquet(
    df_temp_dim$bpq_data_list,
    paste0(.path, '/bpq_data_list.parquet')
  )

  arrow::write_parquet(
    df_temp_dim$bpq_data_mode_of_transport,
    paste0(.path, '/bpq_data_mode_of_transport.parquet')
  )

  df$bpq_data <- arrow::open_dataset(paste0(.path, '/bpq_data.parquet'))
  df$bpq_data_list <- arrow::open_dataset(paste0(.path, '/bpq_data_list.parquet'))
  df$bpq_data_mode_of_transport <- arrow::open_dataset(paste0(.path, '/bpq_data_mode_of_transport.parquet'))

}
