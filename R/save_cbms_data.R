save_cbms_data <- function(
    .df_src_files,
    .input_data,
    .pq_path,
    .p_name,
    .is_first_record = FALSE,
    .chunk = NULL,
    .chunk_size = 1,
    .references = get_config("references"),
    .config = getOption("rcbms.config")) {
  envir <- as.environment(1)
  geo_cols <- c("region_code", "province_code", "city_mun_code", "barangay_code")
  uid <- "case_id"
  if (.input_data == "bp") uid <- "barangay_geo"

  rov_var <- .config$project[[.input_data]]$variable$result_of_visit
  unfiltered_records <- .config$project[[.input_data]]$unfiltered_records

  df_list <- lapply(.df_src_files, function(x) {
    suppressWarnings(
      import_data(x, .input_data = .input_data) |>
        clean_colnames() |>
        harmonize_variable(
          .references$data_dictionary,
          .survey_round = .config$survey_round,
          .input_data = .input_data
        )
    )
  })

  df_temp <- dplyr::bind_rows(df_list)

  df_temp_dim_before <- c(nrow(df_temp), ncol(df_temp))
  attr(df_temp, "dim_before_tidy") <- df_temp_dim_before

  if (.is_first_record) {
    summary_record <- df_temp |>
      create_case_id(.input_data = .input_data) |>
      dplyr::select(dplyr::any_of(c(uid, geo_cols, rov_var)))

    assign("summary_record", summary_record, envir = envir)
  }

  if (exists("summary_record") & isFALSE(.is_first_record)) {
    with_geo_code <- which(geo_cols %in% names(df_temp))

    if (length(with_geo_code) == 4) {
      summary_record_only <- summary_record |>
        dplyr::select(-dplyr::any_of(geo_cols))
    } else {
      summary_record_only <- summary_record
    }

    df_temp <- df_temp |>
      create_case_id(.input_data = .input_data) |>
      dplyr::left_join(summary_record_only, by = uid)

    if (.config$complete_cases && !(.p_name %in% unfiltered_records)) {
      if (rov_var %in% names(df_temp)) {
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

  assign("df_temp", df_temp, envir = envir)

  src_file <- join_path(.config$base, "tidy", .input_data, paste0(.p_name, ".R"))
  if (file.exists(src_file)) source(src_file)

  if (exists("df_temp_tidy")) df_temp <- df_temp_tidy

  df_temp <- df_temp |>
    add_metadata(
      .dictionary = .references$data_dictionary,
      .valueset = .references$valueset,
      .survey_round = .config$survey_round,
      .input_data = .input_data
    ) |>
    dplyr::select(
      dplyr::any_of(
        c(
          uid,
          geo_cols,
          "ean",
          "bsn",
          "husn",
          "hsn",
          "line_number",
          rov_var,
          "sex",
          "age"
        )
      ),
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

  arrow::write_parquet(df_temp |> create_case_id(.input_data = .input_data), .pq_path)

  suppressWarnings(rm(list = "df_temp_tidy", envir = envir))
  suppressWarnings(rm(list = "df_temp", envir = envir))

  if (!is.null(.chunk) & .chunk_size > 1) {
    .chunk <- paste0("[", .chunk, "/", .chunk_size, "] ")
  }

  if (.config$verbose) {
    cli::cli_alert_info(
      paste0(
        "Importing ", .chunk, cli::col_br_yellow(.p_name), " record ", df_temp_dim, cli::col_br_cyan("✓")
      )
    )
  }
}
