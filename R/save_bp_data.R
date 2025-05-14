save_bp_data <- function(.conn, .pq_folder, .references, .config) {

  df <- list()

  geo_cols_name <- c("region", "province", "city_mun", "barangay")
  geo_cols <- paste0(geo_cols_name, "_code")

  if(.config$project$add_length == 0) {
    anm_src <- 'area_name'
  } else if(.config$project$add_length == 1) {
    anm_src <- 'area_name_new'
  } else {
    stop('Invalid add_length argument.')
  }

  dcf <- .references$data_dictionary[["2024"]][["bp"]]
  if(tolower(.config$mode$stage[1]) > 3) {
    dcf <- dcf |> dplyr::mutate(variable_name = variable_name_new)
  }

  df <- list()
  df_temp <- read_bp_data(dcf, .config)
  df_bp_names <- names(df_temp)

  for(i in seq_along(df_bp_names)) {

    bp_i <- df_bp_names[i]
    pq_i <- file.path(.pq_folder, paste0(bp_i, ".parquet"))

    df_temp_i <- df_temp[[bp_i]] |>
      create_barangay_geo() |>
      dplyr::select(-dplyr::any_of(geo_cols_name)) |>
      dplyr::left_join(
        transform_area_name(.references[[anm_src]], .config$project$add_length) |>
          dplyr::select(
            barangay_geo,
            dplyr::any_of(
              c(
                geo_cols_name,
                'total_population',
                'total_hh',
                'average_hh_size',
                'is_huc',
                '2020_popn',
                'class'
              )
            )
          ),
        by = "barangay_geo"
      ) |>
      dplyr::select(
        dplyr::any_of(c("barangay_geo", geo_cols, geo_cols_name)),
        sort(names(df_temp[[bp_i]]))
      ) |>
      tibble::tibble() |>
      dplyr::mutate(row_id = dplyr::row_number(), .before = 1) |>
      sort_variable_names('bp', .config) |>
      add_metadata(dcf, .references$valueset)

    if(.config$use_encryption) {

      q_to_pq <- paste0(
        "COPY df_temp TO '",
        pq_i,
        "' (ENCRYPTION_CONFIG { footer_key: '", .config$env$AES_KEY, "' });"
      )

      DBI::dbWriteTable(.conn, name = "df_temp", value = df_temp_i, overwrite = T)
      DBI::dbExecute(.conn, q_to_pq)

      q_pq <- paste0(
        "SELECT * FROM read_parquet('",
        pq_i,
        "', encryption_config = { footer_key: '",
        .config$env$AES_KEY,
        "' });"
      )

      df[[bp_i]] <- DBI::dbGetQuery(.conn, q_pq) |>
        add_metadata(dcf, .references$valueset) |>
        arrow::arrow_table()

    } else {

      arrow::write_parquet(df_temp_i, pq_i)
      df[[bp_i]] <- arrow::open_dataset(pq_i)
    }
  }

  return(df)

}
