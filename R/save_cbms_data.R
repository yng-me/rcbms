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

  current_project <- .config$project[[.input_data]]
  uid <- current_project$id
  if(.config$project$add_length == 0) {
    anm_src <- 'area_name'
  } else if(.config$project$add_length == 1) {
    anm_src <- 'area_name_new'
  } else {
    stop('Invalid add_length argument.')
  }

  geo_cols_name <- c("region", "province", "city_mun", "barangay")
  geo_cols <- paste0(geo_cols_name, "_code")
  sn_cols <- c("ean", "bsn", "husn", "hsn", "line_number")

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

  final_status <- current_project$final_status
  final_status_vars <- final_status$variable
  unfiltered_records <- current_project$unfiltered_records

  if(.is_first_record) {

    df_temp <- df_temp |>
      dplyr::select(-dplyr::any_of(geo_cols_name)) |>
      create_case_id(.input_data = .input_data) |>
      create_barangay_geo() |>
      dplyr::select(
        dplyr::any_of(c(uid, geo_cols, "barangay_geo", final_status_vars)),
        dplyr::everything()
      )

  } else {

    if(!is.null(.summary_record)) {

      with_geo_code <- which(geo_cols %in% names(df_temp))

      if(length(with_geo_code) == 4) {
        summary_record_only <- .summary_record |>
          dplyr::select(dplyr::any_of(c(uid, "barangay_geo", final_status_vars)))

      } else {
        summary_record_only <- .summary_record |>
          dplyr::select(dplyr::any_of(c(uid, geo_cols, "barangay_geo", final_status_vars)))
      }

      df_temp <- df_temp |>
        dplyr::select(-dplyr::any_of(geo_cols_name)) |>
        create_case_id(.input_data = .input_data) |>
        dplyr::left_join(summary_record_only, by = uid)

    }

    if(.config$complete_cases & !(.p_name %in% unfiltered_records)) {

      if(.config$mode$type != 'validation' &
        !is.null(final_status) &
        !is.null(final_status_vars) &
        !is.null(final_status$value) &
        !is.null(final_status$index)
        ) {

        final_status_var <- final_status$variable[[final_status$index]]

        if(final_status_var %in% names(df_temp)) {
          df_temp <- df_temp |>
            dplyr::filter(
              tolower(as.character(!!as.name(final_status_var))) == tolower(as.character(final_status$value))
            )
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
  }

  if('barangay_geo' %in% names(df_temp)) {

    df_temp <- df_temp |>
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
      )

    df_temp <- sort_variable_names(df_temp, .input_data, .config)
  }

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

  if(.config$parquet$encrypt &
     !is.null(.config$env$PQ_KEY_PUB) &
     !is.null(.config$env$PQ_KEY_PRV)
  ) {

    df_temp <- df_temp |>
      create_case_id(.input_data = .input_data) |>
      tibble::tibble()

    key_pub <- .config$env$PQ_KEY_PUB
    q_to_pq <- paste0(
      "COPY df_temp TO '",
      .pq_path ,
      "' (ENCRYPTION_CONFIG {footer_key: '", key_pub, "'});"
    )

    DBI::dbWriteTable(.conn, name = "df_temp", value = df_temp, overwrite = T)
    DBI::dbExecute(.conn, q_to_pq)

  } else {

    df_temp <- df_temp |>
      add_metadata(dcf, .references$valueset) |>
      create_case_id(.input_data = .input_data) |>
      dplyr::mutate(row_id = dplyr::row_number(), .before = 1)

    chunk <- .config$parquet$partition & !is.null(.config$parquet$partition_by)
    with_cols <- .config$parquet$partition_by %in% names(df_temp)
    with_cols <- length(with_cols[with_cols]) == length(.config$parquet$partition_by)

    if(chunk & with_cols) {

      .pq_path <- stringr::str_remove(.pq_path, '\\.parquet$')
      df_temp <- df_temp |>
        dplyr::group_by(dplyr::pick(dplyr::any_of(.config$parquet$partition_by)))

      print(dplyr::group_vars(df_temp))

      arrow::write_dataset(df_temp, .pq_path, format = 'parquet')
    } else {
      arrow::write_parquet(df_temp |> dplyr::ungroup(), .pq_path)
    }

    df_temp <- df_temp |> dplyr::ungroup()

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
