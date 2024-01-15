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

  input_data <- check_input_data(.config$input_data)
  mode <- tolower(.config$mode$type)

  read_from_parquet <- FALSE
  if(!is.null(.config$read_from_parquet)) {
    read_from_parquet <- .config$read_from_parquet
  }

  df <- list()

  for(i in seq_along(input_data)) {

    df_input <- input_data[i]
    rov_var <- config$project[[df_input]]$variable$result_of_visit

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

    df_files <- list_data_files(df_input, .config)

    uid <- "case_id"
    if(df_input == "bp") uid <- "uuid"

    summary_record <- NULL

    for(j in seq_along(df_files$unique$value)) {

      p <- df_files$unique$value[j]
      file_format <- get_file_format(.config, df_input)
      p_name <- stringr::str_remove(tolower(p), file_format)
      pq_folder <- create_new_folder(get_data_path('parquet', df_input))
      pq_path <- file.path(pq_folder, paste0(p_name, '.parquet'))

      if(!read_from_parquet) {

        df_src_files <- dplyr::as_tibble(df_files$all$value) |>
          dplyr::filter(grepl(paste0(p, '$'), value)) |>
          dplyr::pull(value)

        df_list <- lapply(df_src_files, function(x) {

          suppressWarnings(
            import_data(x, .input_data = df_input) |>
              clean_colnames() |>
              harmonize_variable(
                .references$data_dictionary,
                .cbms_round = .config$cbms_round,
                .input_data = df_input
              )
          )
        })

        df_temp <- do.call('rbind', df_list) |> dplyr::tibble()

        df_temp_dim_before <- c(nrow(df_temp), ncol(df_temp))
        attr(df_temp, "dim_before_tidy") <- df_temp_dim_before

        if(j == 1 && df_files$unique$n[j] == 0) {
          summary_record <- df_temp |>
            create_case_id(.input_data = df_input) |>
            dplyr::select(dplyr::any_of(c(uid, geo_cols, rov_var)))
        }

        if(!is.null(summary_record) && df_files$unique$n[j] > 0) {
          with_geo_code <- which(geo_cols %in% names(df_temp))
          if(length(with_geo_code) == 4) {
            summary_record_only <- summary_record |>
              dplyr::select(-dplyr::any_of(geo_cols))

          } else {
            summary_record_only <- summary_record
          }

          df_temp <- df_temp |>
            create_case_id(.input_data = df_input) |>
            dplyr::left_join(summary_record_only, by = uid)
        }

        assign("df_temp", df_temp, envir = envir)

        src_file <- join_path(.config$base, 'tidy', df_input, paste0(p_name, '.R'))
        if(file.exists(src_file)) source(src_file)

        if(exists("df_temp_tidy")) df_temp <- df_temp_tidy

        df_temp <- df_temp |>
          add_metadata(.references$data_dictionary, .references$valueset) |>
          dplyr::select(dplyr::any_of(c(uid, geo_cols, rov_var)), dplyr::everything())

        df_temp_dim_after <- c(nrow(df_temp), ncol(df_temp))
        attr(df_temp, "dim_after_tidy") <- df_temp_dim_after

        if(.config$verbose) {
          if(identical(df_temp_dim_before, df_temp_dim_after)) {
            df_temp_dim <- paste0(
              cli::col_br_cyan(df_temp_dim_before[1]), "×",
              cli::col_br_cyan(df_temp_dim_before[2])
            )
          } else {
            df_temp_dim <- paste0(
              cli::col_br_cyan(df_temp_dim_before[1]), "×",
              cli::col_br_cyan(df_temp_dim_before[2]), " → ",
              cli::col_br_cyan(df_temp_dim_after[1]), "×",
              cli::col_br_cyan(df_temp_dim_after[2])
            )
          }
          df_temp_dim <- paste0("(", df_temp_dim, ") ")
        }

        arrow::write_parquet(df_temp, pq_path)
        suppressWarnings(rm(list = 'df_temp_tidy', envir = envir))
        suppressWarnings(rm(list = 'df_temp', envir = envir))

      } else {
        df_temp_dim <- ""
      }

      if(.config$verbose) {
        cli::cli_alert_info(
          paste0(
            "Importing ", cli::col_br_yellow(p_name), " record ", df_temp_dim, cli::col_br_cyan("✓")
          )
        )
      }

      df[[df_input]][[p_name]] <- arrow::open_dataset(pq_path)

    }
  }

  df <- set_class(df, "rcbms_parquet")

  if(!is.null(.assign_name)) {

    .config$links$parquet <- .assign_name
    options(rcbms.config = .config)

    assign(.assign_name, df, envir = envir)
  }


  return(invisible(df))
}


get_input_data_label <- function(.key) {
  labels <- c("Household Profile", "Barangay Profile", "Institutional Population")
  labels_short <- c("hp", "bp", "ilq")
  label_which <- which(labels_short == .key)
  if(length(label_which) == 0) return(.key)
  labels[which(labels_short == .key)]
}


# tidy_data_frame <- function(.data, .base, .input_data, .record, ...) {
#
#   src_file <- join_path(.base, 'tidy', .input_data, paste0(.record, '.R'))
#
#   if(!file.exists(src_file)) return(.data)
#   source(src_file)
#
#   if(!exists('tidy_cbms_data_temp')) return(.data)
#
#   .data |> tidy_cbms_data_temp(...)
#
# }
#
#
# tidy_cbms_data <- function(
#     .parquet,
#     .record,
#     .input_data,
#     ...,
#     .config = getOption('rcbms.config'),
#     .complete_cases = NULL
# ) {
#
#   if(exists('tidy_cbms_data_temp')) {
#     suppressWarnings(rm(list = 'tidy_cbms_data_temp'))
#   }
#
#   df <- .parquet[[.record]] |>
#     dplyr::collect() |>
#     create_case_id(.input_data = .input_data)
#
#   if(!is.null(.complete_cases)) {
#     df <- df |> dplyr::filter(case_id %in% .complete_cases)
#   }
#
#   df |> tidy_data_frame(.config$base, .record, .input_data, ...)
#
# }

