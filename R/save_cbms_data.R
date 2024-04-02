save_cbms_data <- function(
  .conn,
  .df_src_files,
  .input_data,
  .pq_path,
  .p_name,
  .references,
  .config,
  .is_first_record = FALSE,
  .summary_record = NULL
) {

  uid <- "case_id"
  if (.input_data == "bp") uid <- "barangay_geo"

  geo_cols_name <- c("region", "province", "city_mun", "barangay")
  geo_cols <- paste0(geo_cols_name, "_code")
  sn_cols <- c("ean", "bsn", "husn", "hsn", "line_number")

  rov_var <- .config$project[[.input_data]]$variable$result_of_visit
  unfiltered_records <- .config$project[[.input_data]]$unfiltered_records

  dcf <- .references$data_dictionary[[.config$survey_round]][[.input_data]]
  if(tolower(.config$mode$stage[1]) > 3) {
    dcf <- dcf |> dplyr::mutate(variable_name = variable_name_new)
  }

  df_list <- lapply(.df_src_files, function(x) {
    if(.config$warning) {
      import_data(x, .input_data, .config) |>
        clean_colnames() |>
        harmonize_variable(
          .dictionary = dcf,
          .survey_round = .config$survey_round,
          .input_data = .input_data,
          .config = .config
        )
    } else {
      suppressWarnings(
        import_data(x, .input_data, .config) |>
          clean_colnames() |>
          harmonize_variable(
            .dictionary = dcf,
            .survey_round = .config$survey_round,
            .input_data = .input_data,
            .config = .config
          )
      )
    }
  })

  df_temp <- dplyr::bind_rows(df_list)

  df_temp_dim_before <- c(nrow(df_temp), ncol(df_temp))
  attr(df_temp, "dim_before_tidy") <- df_temp_dim_before

  if(.is_first_record) {

    df_temp <- df_temp |>
      create_case_id(.input_data = .input_data) |>
      create_barangay_geo() |>
      dplyr::select(
        dplyr::any_of(c(uid, geo_cols, "barangay_geo", rov_var)),
        dplyr::everything()
      )

  } else {

    if(!is.null(.summary_record)) {

      with_geo_code <- which(geo_cols %in% names(df_temp))

      if(length(with_geo_code) == 4) {
        summary_record_only <- .summary_record |>
          dplyr::select(dplyr::any_of(c(uid, "barangay_geo", rov_var)))

      } else {
        summary_record_only <- .summary_record |>
          dplyr::select(dplyr::any_of(c(uid, geo_cols, "barangay_geo", rov_var)))
      }

      df_temp <- df_temp |>
        create_case_id(.input_data = .input_data) |>
        dplyr::left_join(summary_record_only, by = uid)
    }

    if(.config$complete_cases & !(.p_name %in% unfiltered_records)) {

      if(rov_var %in% names(df_temp)) {
        df_temp <- df_temp |>
          dplyr::filter(!!as.name(rov_var) == 1)
      }

      if ("hsn" %in% names(df_temp)) {
        df_temp <- df_temp |>
          dplyr::filter(
            as.integer(hsn) < as.integer(
              paste(rep(7, 4 + .config$project$add_length), collapse = "")
            )
          )
      }
    }
  }

  df_temp <- df_temp |>
    dplyr::left_join(
      transform_area_name(.references$area_name, .config$project$add_length) |>
        dplyr::select(
          barangay_geo,
          dplyr::any_of(c(geo_cols_name, 'is_huc', '2020_popn', 'class'))
        ),
      by = "barangay_geo"
    ) |>
    dplyr::select(
      dplyr::any_of(c(uid, geo_cols, sn_cols, geo_cols_name, rov_var, "sex", "age")),
      sort(names(df_temp))
    )


  df_temp_dim_after <- c(nrow(df_temp), ncol(df_temp))
  attr(df_temp, "dim_after_tidy") <- df_temp_dim_after

  if (.config$verbose) {
    if (identical(df_temp_dim_before, df_temp_dim_after)) {
      df_temp_dim <- paste0(
        cli::col_br_cyan(format(df_temp_dim_before[1], big.mark = ",")), " × ",
        cli::col_br_cyan(format(df_temp_dim_before[2], big.mark = ","))
      )
    } else {
      df_temp_dim <- paste0(
        cli::col_br_cyan(format(df_temp_dim_before[1], big.mark = ",")), " × ",
        cli::col_br_cyan(format(df_temp_dim_before[2], big.mark = ",")), " → ",
        cli::col_br_cyan(format(df_temp_dim_after[1], big.mark = ",")), " × ",
        cli::col_br_cyan(format(df_temp_dim_after[2], big.mark = ","))
      )
    }
    df_temp_dim <- paste0("(", df_temp_dim, ") ")
  }

  if (.input_data %in% c("cph", "bs") & .config$survey_round == "2020") {
    df_temp <- df_temp |>
      dplyr::mutate(
        province_code = stringr::str_sub(province_code, 2, 3)
      )
  }

  if(.config$parquet$encrypt &
     !is.null(.config$env$PQ_KEY_PUB) &
     !is.null(.config$env$PQ_KEY_PRV)
     ) {

    df_temp <- df_temp |>
      create_case_id(.input_data = .input_data) |>
      tibble::tibble()

    key_pub <- .config$env$PQ_KEY_PUB
    q_to_pq <- paste0("COPY df_temp TO '", .pq_path , "' (ENCRYPTION_CONFIG {footer_key: '", key_pub, "'});")

    DBI::dbWriteTable(.conn, name = "df_temp", value = df_temp, overwrite = T)
    DBI::dbExecute(.conn, q_to_pq)

  } else {
    df_temp <- df_temp |>
      add_metadata(dcf, .references$valueset) |>
      create_case_id(.input_data = .input_data)

    arrow::write_parquet(df_temp, .pq_path)
  }

  if (.config$verbose) {
    cli::cli_alert_info(
      paste0(
        "Importing ", cli::col_br_yellow(.p_name), " record ", df_temp_dim, cli::col_br_cyan("✓")
      )
    )
  }

  return(df_temp)

}
