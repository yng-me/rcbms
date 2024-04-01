save_bp_data <- function(.conn, .pq_folder, .references, .config) {

  df <- list()

  geo_cols_name <- c("region", "province", "city_mun", "barangay")
  geo_cols <- paste0(geo_cols_name, "_code")

  dcf <- .references$data_dictionary[["2024"]][["bp"]]
  if(tolower(.config$mode$stage[1]) > 3) {
    dcf <- dcf |> dplyr::mutate(variable_name = variable_name_new)
  }

  use_encryption <- .config$parquet$encrypt & !is.null(.config$env$PQ_KEY_PUB) & !is.null(.config$env$PQ_KEY_PRV)

  df_temp <- read_bp_data(dcf, .config)
  df_bp_names <- names(df_temp)

  if(use_encryption) {

    key_pub <- .config$env$PQ_KEY_PUB

    for(i in seq_along(df_bp_names)) {
      bp_i <- df_bp_names[i]
      pq <- file.path(.pq_folder, paste0(bp_i, ".parquet"))

      df_temp_i <- df_temp[[bp_i]] |>
        create_barangay_geo() |>
        dplyr::left_join(
          transform_area_name(.references$area_name, .config$project$add_length) |>
            dplyr::select(
              barangay_geo,
              dplyr::any_of(c(geo_cols_name, 'is_huc', '2020_popn', 'class'))
            ),
          by = "barangay_geo"
        ) |>
        dplyr::select(
          dplyr::any_of(c("barangay_geo", geo_cols, geo_cols_name)),
          sort(names(df_temp[[bp_i]]))
        ) |>
        tibble::tibble()

      DBI::dbWriteTable(.conn, name = "df_temp", value = df_temp_i, overwrite = T)

      q_to_pq <- paste0("COPY df_temp TO '", pq , "' (ENCRYPTION_CONFIG {footer_key: '", key_pub, "'});")
      DBI::dbExecute(.conn, q_to_pq)

      pq_query <- paste0("SELECT * FROM read_parquet('", pq, "', encryption_config = { footer_key: '", key_pub, "' });")

      df[[bp_i]] <- DBI::dbGetQuery(.conn, pq_query) |>
        add_metadata(dcf, .references$valueset)

    }


  } else {

    arrow::write_parquet(
      df_temp$bpq_data,
      file.path(.pq_folder, 'bpq_data.parquet')
    )
    arrow::write_parquet(
      df_temp$bpq_data_list,
      file.path(.pq_folder, 'bpq_data_list.parquet')
    )

    arrow::write_parquet(
      df_temp$bpq_data_mode_of_transport,
      file.path(.pq_folder, 'bpq_data_mode_of_transport.parquet')
    )

    df$bpq_data <- arrow::open_dataset(file.path(.pq_folder, 'bpq_data.parquet'))
    df$bpq_data_list <- arrow::open_dataset(file.path(.pq_folder, 'bpq_data_list.parquet'))
    df$bpq_data_mode_of_transport <- arrow::open_dataset(file.path(.pq_folder, 'bpq_data_mode_of_transport.parquet'))

  }

  return(df)

}
