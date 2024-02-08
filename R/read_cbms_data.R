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

read_cbms_data <- function(
  .references = get_config("references"),
  .config = getOption('rcbms.config'),
  .assign_name = "parquet"
) {

  if(.config$verbose) {
    cli::cli_h1("Importing Data Files")
  }

  envir <- as.environment(1)
  geo_cols <- c("region_code", "province_code", "city_mun_code", "barangay_code")
  summary_record <- NULL

  input_data <- check_input_data(.config$input_data)
  mode <- tolower(.config$mode$type)
  chunk_threshold <- .config$chunk_threshold
  if(is.null(chunk_threshold)) {
    chunk_threshold <- 4
  }

  read_from_parquet <- FALSE
  if(!is.null(.config$read_from_parquet)) {
    read_from_parquet <- .config$read_from_parquet
  }

  df <- list()

  for(i in seq_along(input_data)) {

    df_input <- input_data[i]

    df_files <- list_data_files(df_input, .references, .config)

    if(!read_from_parquet) {
      rov_var <- config$project[[df_input]]$variable$result_of_visit
      unfiltered_records <- .config$project[[df_input]]$unfiltered_records
      if(is.null(unfiltered_records)) unfiltered_records <- ""

      if(.config$verbose) {
        if(length(input_data) > 1) {
          progress_n <- paste0("[", i, "/", length(input_data), "]: ")
        } else {
          progress_n <- ""
        }
        cli::cli_h3(
          paste0(progress_n, cli::col_br_cyan(get_input_data_label(df_input)))
        )
      }

      uid <- "case_id"
      if(df_input == "bp") uid <- "uuid"
    }

    for(j in seq_along(df_files$unique$value)) {

      p <- df_files$unique$value[j]
      file_format <- get_file_format(.config, df_input)
      pq_folder <- create_new_folder(get_data_path('parquet', df_input))

      if(read_from_parquet) {
        p_name <- stringr::str_remove(tolower(basename(p)), "\\.parquet$")
      } else {
        p_name <- stringr::str_remove(tolower(basename(p)), file_format)
      }

      pq_path <- file.path(pq_folder, paste0(p_name, ".parquet"))

      if(!read_from_parquet) {

        df_src_files <- df_files$all |>
          dplyr::filter(grepl(paste0(p, '$'), value))

        file_size <- sum(df_src_files$size) / 1000000
        n_files <- length(df_src_files$value)
        n_chunks <- as.integer(round(file_size / (n_files * 7)))
        is_first_record <- j == 1 & df_files$unique$n[j] == 0

        if(file_size <  5000 || n_chunks < 2) {

          df_temp_dim <- save_cbms_data(
            .df_src_files = df_src_files$value,
            .is_first_record = is_first_record,
            .input_data = df_input,
            .pq_path = pq_path,
            .p_name = p_name
          )

        } else {

          chuck_start <- 1
          pq_path_chunck <- create_new_folder(file.path(pq_folder, "chunk"))

          for(k in seq_len(n_chunks)) {

            chunk_counter <- stringr::str_pad(k, width = 4, pad = "0")
            chunk_name <- paste0(p_name, '__', chunk_counter, '.parquet')
            chunk_pq <- paste0(pq_path_chunck, '/', chunk_name)

            chuck_end <- round((n_files / n_chunks) * k)
            if(k == n_chunks) chuck_end <- n_files

            files_to_chunk <- df_src_files$value[chuck_start:chuck_end]

            df_temp_dim <- save_cbms_data(
              .df_src_files = files_to_chunk,
              .is_first_record = is_first_record,
              .input_data = df_input,
              .pq_path = chunk_pq,
              .chunk = k,
              .chunk_size = n_chunks,
              .p_name = p_name
            )

            chuck_start <- chuck_end + 1

            df[[df_input]][[p_name]][[chunk_counter]] <- arrow::open_dataset(chunk_pq)
          }
        }

      }

      chunk_path <- paste0(pq_folder, '/chunk')

      if(dir.exists(chunk_path)) {

        chunk_file_paths <- list.files(chunk_path, full.names = TRUE)
        chunk_file_paths <- chunk_file_paths[grepl(p_name, chunk_file_paths)]

        for(m in seq_along(chunk_file_paths)) {
          chunk_file <- chunk_file_paths[m]

          chunk_counter <- stringr::str_extract_all(chunk_file, "\\d{4}\\.parquet$")[[1]] |>
            stringr::str_remove("\\.parquet$")

          df[[df_input]][[p_name]][[chunk_counter]] <- arrow::open_dataset(chunk_file)

        }
      }

      if(read_from_parquet & .config$verbose) {
        cli::cli_alert_info(
          paste0(
            "Importing ", cli::col_br_yellow(p_name), " record ", cli::col_br_cyan("✓")
          )
        )
      }

      if(file.exists(pq_path)) {
        df[[df_input]][[p_name]] <- arrow::open_dataset(pq_path)
      }
    }
  }

  df <- set_class(df, "rcbms_parquet")

  if(!is.null(.assign_name)) {

    .config$links$parquet <- .assign_name
    options(rcbms.config = .config)

    assign(.assign_name, df, envir = envir)
  }

  suppressWarnings(rm(list = 'summary_record', envir = envir))

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
  if(length(label_which) == 0) return(.key)
  labels[which(labels_short == .key)]
}
