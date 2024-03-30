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

  geo_cols <- c("region_code", "province_code", "city_mun_code", "barangay_code")

  input_data <- check_input_data(.config$input_data)
  mode <- tolower(.config$mode$type)

  convert_to_parquet <- TRUE
  if(!is.null(.config$parquet$convert)) {
    convert_to_parquet <- .config$parquet$convert
  }

  summary_record <- NULL

  df <- list()

  for(i in seq_along(input_data)) {

    df_input <- input_data[i]

    is_bp_data <- df_input == 'bp' & as.integer(.config$survey_round) == 2024

    df_files <- list_data_files(df_input, .references, .config)

    if(convert_to_parquet) {
      if(!is_bp_data) {

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
      }

      uid <- "case_id"
      if(df_input == "bp") uid <- "barangay_geo"
    }

    for(j in seq_along(df_files$unique$value)) {

      p <- df_files$unique$value[j]
      file_format <- get_file_format(.config, df_input)
      pq_folder <- create_new_folder(get_data_path('parquet', df_input, .config))

      if(!convert_to_parquet) {
        p_name <- stringr::str_remove(tolower(basename(p)), "\\.parquet$")
      } else {
        p_name <- stringr::str_remove(tolower(basename(p)), file_format)
      }

      pq_path <- file.path(pq_folder, paste0(p_name, ".parquet"))

      if(convert_to_parquet) {

        df_src_files <- df_files$all |>
          dplyr::filter(grepl(paste0(p, '$'), value))

        is_first_record <- j == 1 & df_files$unique$n[j] == 0

        if(is_bp_data) {

          bp_base_path <- config$project$bp$directory
          if(is.null(bp_base_path)) {
            bp_base_path <- paste0(.config$base, '/data/raw/bp')
          }

          df_temp_dim <- read_bp_data(bp_base_path, .references, .config)
          df$bp <- save_bp_data(df_temp_dim, './src/2024/data/parquet/bp')

        } else {

          df_temp_dim <- save_cbms_data(
            .df_src_files = df_src_files$value,
            .is_first_record = is_first_record,
            .input_data = df_input,
            .pq_path = pq_path,
            .p_name = p_name,
            .config = .config,
            .references = .references
          )
        }
      }

      if(!convert_to_parquet & .config$verbose) {
        cli::cli_alert_info(
          paste0(
            "Importing ", cli::col_br_yellow(p_name), " record ", cli::col_br_cyan("✓")
          )
        )
      }

      if(.config$progress) {
        cli::cli_text(paste0('Importing ', p_name, ' record'))
      }

      if(file.exists(pq_path)) {
        df[[df_input]][[p_name]] <- arrow::open_dataset(pq_path)
      }
    }
  }

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
  if(length(label_which) == 0) return(.key)
  labels[which(labels_short == .key)]
}
