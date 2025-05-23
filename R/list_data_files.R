#' Title
#'
#' @param .input_data
#' @param .references
#' @param .config
#'
#' @return
#' @export
#'
#' @examples
#'

list_data_files <- function(.input_data, .references, .config) {

  if(is.null(.config)) stop('.config not defined.')

  file_format <- get_file_format(.config, .input_data)
  convert_to_parquet <- .config$parquet$convert
  partition <- .config$parquet$partition
  df_input_folder <- "raw"

  if(!convert_to_parquet) df_input_folder <- "parquet"

  if (!is.null(.config$project[[.input_data]]$directory) & convert_to_parquet) {
    input_data_path <- .config$project[[.input_data]]$directory
  } else {
    input_data_path <- get_data_path(df_input_folder, .input_data, .config)
  }

  if (!dir.exists(input_data_path)) {
    stop("Data directory does not exist: ", input_data_path)
  }

  summary_record <- .config$project[[.input_data]][['summary_record']]

  if (is.null(summary_record)) summary_record <- "~~~"
  if (is.na(summary_record)) summary_record <- "~~~"


  if(!convert_to_parquet) {

    if(partition) {

      file_list <- list(
        unique = list.files(input_data_path) |>
          dplyr::as_tibble() |>
          dplyr::distinct(value, .keep_all = T) |>
          dplyr::mutate(
            name = tolower(basename(value)),
            n = seq(1:dplyr::n()),
            n = dplyr::if_else(grepl(paste0("^", summary_record), name), 0L, n)
          ) |>
          dplyr::arrange(n)
      )

    } else {

      recursive <- .config$convert$recursive
      if(is.null(recursive)) {
        recursive <- TRUE
      }

      file_list <- list(
        unique = list.files(
          input_data_path,
          pattern = "\\.parquet$",
          recursive = recursive
        ) |>
        dplyr::as_tibble() |>
        dplyr::filter(grepl("\\.parquet$", value, ignore.case = T)) |>
        dplyr::mutate(
          value = stringr::str_remove_all(value, "__\\d{4}\\.parquet$")
        ) |>
        dplyr::distinct(value, .keep_all = T) |>
        dplyr::mutate(
          name = tolower(stringr::str_remove(basename(value), "\\.parquet$")),
          n = seq(1:dplyr::n()),
          n = dplyr::if_else(grepl(paste0("^", summary_record), name), 0L, n)
        ) |>
        dplyr::arrange(n)
      )
    }

    return(file_list)
  }

  all_data_files <- list.files(
    input_data_path,
    pattern = file_format,
    recursive = !partition,
    full.names = T,
    include.dirs = partition
  ) |>
    dplyr::as_tibble() |>
    dplyr::filter(grepl(file_format, value, ignore.case = T)) |>
    dplyr::mutate(file.info(value))

  if(nrow(all_data_files) == 0 & .config$parquet$convert) {
    stop(paste0('No input data files found for ', .input_data))
  }

  data_files <- dplyr::as_tibble(basename(all_data_files$value)) |>
    dplyr::filter(grepl(file_format, value, ignore.case = T)) |>
    dplyr::distinct() |>
    dplyr::mutate(
      name = tolower(stringr::str_remove(value, file_format)),
      n = seq(1:dplyr::n()),
      n = dplyr::if_else(grepl(paste0("^", summary_record), name), 0L, n)
    ) |>
    dplyr::arrange(n)

  selected_records <- .references$section[[.config$survey_round]][[.input_data]]

  if(!is.null(selected_records)) {

    selected_records <- selected_records |>
      dplyr::filter(included) |>
      dplyr::pull(validation.record) |>
      unlist() |>
      unique() |>
      tolower() |>
      stringr::str_trim()
  }

  ref_records <- .references$record[[.config$survey_round]][[.input_data]]

  if(!is.null(ref_records)) {
    ref_records <- ref_records |>
      dplyr::filter(type > 0 | include == 1) |>
      dplyr::pull(record_name) |>
      tolower() |>
      stringr::str_trim()
  }

  records_filter <- unique(c(selected_records, ref_records))

  if(length(records_filter) > 0) {
    data_files <- data_files |>
      dplyr::filter(tolower(name) %in% records_filter)

    all_data_files <- all_data_files |>
      dplyr::mutate(name = tolower(stringr::str_remove(basename(value), file_format))) |>
      dplyr::filter(tolower(name) %in% records_filter)
  }

  mode <- .config$mode$type
  edit <- .config$mode$edit
  hp_cv <- .input_data == 'hp' & mode == 'validation'

  if(hp_cv & edit == 0) {
    all_data_files <- all_data_files |>
      dplyr::filter(
        grepl('section_a_e|cover_page|section_o', name)
      )

    data_files <- data_files |>
      dplyr::filter(
        grepl('section_a_e|cover_page|section_o', name)
      )
  }

  if(hp_cv & edit == 3) {
    all_data_files <- all_data_files |>
      dplyr::filter(
        grepl('section_a_e|cover_page', name)
      )

    data_files <- data_files |>
      dplyr::filter(
        grepl('section_a_e|cover_page', name)
      )
  }


  return(
    list(
      all = all_data_files,
      unique = data_files
    )
  )
}


get_file_format <- function(.config, .input_data) {
  if (!is.null(.config$project[[.input_data]][["file_format"]])) {
    file_format <- .config$project[[.input_data]][["file_format"]]
    file_format <- paste0("\\.(", toupper(file_format), "|", tolower(file_format), ")$")
  } else {
    file_format <- "\\.(TXT|txt)$"
  }
  return(file_format)
}


get_data_path <- function(
  .type,
  .input_data,
  .config
) {
  wd <- .config$working_directory

  if (is.null(wd)) wd <- "."
  full_path <- file.path(
    wd,
    "src",
    .config$survey_round,
    "data",
    .type,
    .input_data
  )

  if(exists('CURRENT_AREA_TEMP__')) {
    if (!is.null(CURRENT_AREA_TEMP__)) {
      full_path <- file.path(full_path, CURRENT_AREA_TEMP__)
    }
  }

  return(full_path)
}
