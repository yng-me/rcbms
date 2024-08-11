#' Title
#'
#' @param .references
#' @param .config
#' @param .assign_name
#'
#' @return
#' @export
#'
#' @examples
#'

read_cbms_data <- function(.references, .config) {

  if(.config$verbose) {
    cli::cli_h1("Importing Data Files")
  }

  convert_to_parquet <- TRUE
  if(!is.null(.config$parquet$convert)) {
    convert_to_parquet <- .config$parquet$convert
  }

  partition <- .config$parquet$partition

  duckdb_conn <- NULL
  use_encryption <- .config$parquet$encrypt &
    !is.null(.config$env$AES_KEY) &
    !is.null(.config$env$AES_IV)

  if(use_encryption) {

    duckdb_conn <- DBI::dbConnect(duckdb::duckdb())
    aes_key <- .config$env$AES_KEY
    aes_iv <- .config$env$AES_IV
    q_encrypt <- paste0("PRAGMA add_parquet_key('", aes_key, "', '", aes_iv, "');")
    DBI::dbExecute(duckdb_conn, q_encrypt)

  }

  df <- list()

  for(i in seq_along(.config$input_data)) {

    input_data <- .config$input_data[i]

    is_bp_data <- input_data == "bp" & as.character(.config$survey_round) == "2024"
    is_shp_data <- input_data == "shp" & as.character(.config$survey_round) == "2024"

    df_files <- list_data_files(input_data, .references, .config)
    pq_folder <- create_new_folder(get_data_path("parquet", input_data, .config))

    if(convert_to_parquet) {

      if(!is_bp_data | !is_shp_data) {

        if (.config$verbose) {
          if (length(input_data) > 1) {
            progress_n <- paste0("[", i, "/", length(input_data), "]: ")
          } else {
            progress_n <- ""
          }
          cli::cli_h3(
            paste0(progress_n, cli::col_br_cyan(get_input_data_label(input_data)))
          )
        }
      }
    }

    if(is_bp_data & convert_to_parquet) {

      df$bp <- save_bp_data(duckdb_conn, pq_folder, .references, .config)

    } else if (is_shp_data & convert_to_parquet) {

      df$shp <- save_shp_data(duckdb_conn, pq_folder, .references, .config)

    } else {

      summary_record <- NULL
      file_format <- get_file_format(.config, input_data)

      for(j in seq_along(df_files$unique$value)) {

        p_file <- df_files$unique$value[j]
        p_name <- set_file_name(p_file, file_format, convert_to_parquet)
        pq_dir <- file.path(pq_folder, p_name)
        pq_path <- file.path(pq_folder, paste0(p_name, ".parquet"))

        if(convert_to_parquet) {

          df_src_files <- df_files$all |>
            dplyr::filter(grepl(paste0(p_file, '$'), value))

          is_first_record <- j == 1 & df_files$unique$n[j] == 0

          df_temp <- save_cbms_data(
            .conn = duckdb_conn,
            .df_src_files = df_src_files$value,
            .input_data = input_data,
            .pq_path = pq_path,
            .p_name = p_name,
            .config = .config,
            .references = .references,
            .is_first_record = is_first_record,
            .summary_record = summary_record
          )

          if(is_first_record) {
            summary_record <- df_temp
          }
        }

        if(!convert_to_parquet & .config$verbose) {
          cli::cli_alert_info(
            paste0("Importing ", cli::col_br_yellow(p_name), " record ", cli::col_br_cyan("✓"))
          )
        }

        if(.config$progress) {
          cli::cli_text(paste0('Importing ', p_name, ' record'))
        }

        if(file.exists(pq_path) | file.exists(pq_dir)) {

          if(use_encryption) {

            q_pq <- paste0(
              "SELECT * FROM read_parquet('",
              pq_path,
              "', encryption_config = { footer_key: '",
              aes_key,
              "' });"
            )

            dcf <- .references$data_dictionary[[.config$survey_round]][[input_data]]
            if(tolower(.config$mode$stage[1]) > 3) {
              dcf <- dcf |> dplyr::mutate(variable_name = variable_name_new)
            }

            df[[input_data]][[p_name]] <- DBI::dbGetQuery(duckdb_conn, q_pq) |>
              add_metadata(dcf, .references$valueset) |>
              arrow::arrow_table()

          } else {

            if(partition) {
              df[[input_data]][[p_name]] <- arrow::open_dataset(pq_dir)

            } else {
              df[[input_data]][[p_name]] <- arrow::open_dataset(pq_path)

            }
          }
        }
      }

      if(convert_to_parquet & input_data == "hp" & .config$parquet$delete_source) {
        raw_data_path <- get_data_path("raw", input_data, .config)
        unlink(raw_data_path, recursive = T, force = T)
        create_new_folder(raw_data_path)
      }
    }
  }


  if(use_encryption) DBI::dbDisconnect(duckdb_conn, shutdown = TRUE)
  df <- set_class(df, "rcbms_parquet")

  return(invisible(df))
}


get_input_data_label <- function(.key) {
  labels <- c(
    "Household Profile",
    "Barangay Profile",
    "Institutional Population",
    "Census of Population",
    "Barangay Schedule"
  )
  labels_short <- c("hp", "bp", "ilq", "cph", "bs")
  label_which <- which(labels_short == .key)
  if (length(label_which) == 0) {
    return(.key)
  }
  labels[which(labels_short == .key)]
}


set_file_name <- function(.file, .file_format, .convert_to_parquet) {
  if(.convert_to_parquet) {
    stringr::str_remove(tolower(basename(.file)), .file_format)
  } else {
    stringr::str_remove(tolower(basename(.file)), "\\.parquet$")
  }
}
