save_shp_data <- function(.conn, .pq_folder, .references, .config) {

  dcf <- .references$data_dictionary[["2024"]][["shp"]]
  if(tolower(.config$mode$stage[1]) > 3) {
    dcf <- dcf |> dplyr::mutate(variable_name = variable_name_new)
  }

  # use_encryption <- .config$parquet$encrypt &
  #   !is.null(.config$env$PQ_KEY_PUB) &
  #   !is.null(.config$env$PQ_KEY_PRV)

  shp_base_path <- .config$project$shp$directory
  if(is.null(shp_base_path)) {
    shp_base_path <- file.path(.config$base, "data", "raw", "shp")
  }

  file_format <- paste0(
    "\\.(",
    paste(.config$project$shp$file_format, collapse = '|'),
    ")$"
  )

  # level_length <- c(2, 3, 5, 8)[config$project$shp$aggregation$level]

  shp_data_files <- list.files(
      shp_base_path,
      full.names = T,
      pattern = file_format,
      ignore.case = T
    ) |>
    dplyr::as_tibble() |>
    dplyr::filter(!grepl('[$]', value)) |>
    # dplyr::filter(grepl('^\\d{10}', basename(value))) |>
    dplyr::pull(value)

  df_list <- list()

  for(i in seq_along(shp_data_files)) {

    df_temp <- sf::st_read(shp_data_files[i]) |>
      dplyr::tibble() |>
      janitor::clean_names()

    df_list[[i]] <- df_temp |>
      harmonize_variable(
        .dictionary = dcf,
        .survey_round = .config$survey_round,
        .input_data = "shp",
        .config = .config
      )
  }

  arrow::write_parquet(
    df_list |>
      dplyr::bind_rows() |>
      add_metadata(dcf, .references$valueset),
    file.path(.pq_folder, 'shp_data.parquet')
  )

  arrow::open_dataset(file.path(.pq_folder, 'shp_data.parquet'))

}
