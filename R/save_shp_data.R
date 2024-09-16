save_shp_data <- function(.conn, .pq_folder, .references, .config) {

  dcf <- .references$data_dictionary[["2024"]][["shp"]]
  if(tolower(.config$mode$stage[1]) > 3) {
    dcf <- dcf |> dplyr::mutate(variable_name = variable_name_new)
  }

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
    dplyr::pull(value)

  df_list <- list()

  for(i in seq_along(shp_data_files)) {

    shp_layer <- stringr::str_remove_all(basename(shp_data_files[i]), file_format)

    df_temp <- sf::st_read(shp_data_files[i], layer = shp_layer) |>
      dplyr::tibble() |>
      janitor::clean_names() |>
      suppressWarnings()

    df_list[[i]] <- df_temp |>
      harmonize_variable(
        .dictionary = dcf,
        .survey_round = .config$survey_round,
        .input_data = "shp",
        .config = .config
      )
  }

  df_temp <- df_list |>
    dplyr::bind_rows() |>
    dplyr::mutate(
      cbms_geoid = paste0(geocode, bsn),
      province_code = stringr::str_sub(geocode, 1, 3),
      city_mun_code = stringr::str_sub(geocode, 4, 5),
      barangay_code = stringr::str_sub(geocode, 6, 8),
      ean = stringr::str_sub(geocode, 9, 14),
      .before = 1
    ) |>
    dplyr::left_join(
      .references$area_name_new |>
        dplyr::distinct(region_code, province_code),
      by = 'province_code'
    ) |>
    sort_variable_names('shp', .config) |>
    add_metadata(dcf, .references$valueset) |>
    tibble::tibble() |>
    dplyr::mutate(row_id = dplyr::row_number(), .before = 1)

  pq <- file.path(.pq_folder, 'shp_data.parquet')

  if(.config$use_encryption) {

    q_to_pq <- paste0(
      "COPY df_temp TO '",
      pq,
      "' (ENCRYPTION_CONFIG { footer_key: '", .config$env$AES_KEY, "' });"
    )

    DBI::dbWriteTable(.conn, name = "df_temp", value = df_temp, overwrite = T)
    DBI::dbExecute(.conn, q_to_pq)

    q_pq <- paste0(
      "SELECT * FROM read_parquet('",
      pq,
      "', encryption_config = { footer_key: '",
      .config$env$AES_KEY,
      "' });"
    )

    df <- DBI::dbGetQuery(.conn, q_pq) |>
      add_metadata(dcf, .references$valueset) |>
      arrow::arrow_table()

  } else {

    arrow::write_parquet(df_temp, pq)
    df <- arrow::open_dataset(pq)
  }


  return(df)
#
#
#   arrow::write_parquet(
#     df_temp,
#     file.path(.pq_folder, 'shp_data.parquet')
#   )
#
#   arrow::open_dataset(file.path(.pq_folder, 'shp_data.parquet'))

}
