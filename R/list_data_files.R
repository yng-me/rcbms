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

list_data_files <- function(
  .input_data,
  .references = get_config('references'),
  .config = getOption('rcbms.config')
) {

  if(is.null(.config)) stop('.config not defined.')

  file_format <- get_file_format(.config, .input_data)
  read_from_parquet <- .config$read_from_parquet
  df_input_folder <- "raw"

  if(read_from_parquet) df_input_folder <- "parquet"

  if(!is.null(.config$project$directory)) {
    input_data_path <- .config$project$directory
  } else {
    input_data_path <- get_data_path(df_input_folder, .input_data)
  }

  if(!dir.exists(input_data_path)) {
    stop("Data directory does not exist: ", input_data_path)
  }

  summary_record <- get_summary_record(.input_data)

  if(is.null(summary_record)) summary_record <- '~~~'
  if(is.na(summary_record)) summary_record <- '~~~'

  if(read_from_parquet) {
    return(
      list(
        unique = list.files(input_data_path, pattern = "\\.parquet$", recursive = T) |>
          dplyr::as_tibble() |>
          dplyr::filter(grepl("\\.parquet$", value, ignore.case = T)) |>
          dplyr::mutate(
            value = stringr::str_remove_all(value, "__\\d{4}\\.parquet$")
          ) |>
          dplyr::distinct(value, .keep_all = T) |>
          dplyr::mutate(
            name = tolower(stringr::str_remove(basename(value), "\\.parquet$")),
            n = seq(1:dplyr::n()),
            n = dplyr::if_else(grepl(paste0('^', summary_record), name), 0L, n)
          ) |>
          dplyr::arrange(n)
      )
    )
  }

  all_data_files <- list.files(
      input_data_path,
      pattern = file_format,
      recursive = T,
      full.names = T
    ) |>
    dplyr::as_tibble() |>
    dplyr::filter(grepl(file_format, value, ignore.case = T)) |>
    dplyr::mutate(file.info(value))

  if(nrow(all_data_files) == 0 & !.config$read_from_parquet) {
    stop(paste0('No input data files found for ', .input_data))
  }

  data_files <- dplyr::as_tibble(basename(all_data_files$value)) |>
    dplyr::filter(grepl(file_format, value, ignore.case = T)) |>
    dplyr::distinct() |>
    dplyr::mutate(
      name = tolower(stringr::str_remove(value, file_format)),
      n = seq(1:dplyr::n()),
      n = dplyr::if_else(grepl(paste0('^', summary_record), name), 0L, n)
    ) |>
    dplyr::arrange(n)


  if(!is.null(.references$record)) {
    ref_record <- .references$record |>
      dplyr::filter(
        input_data == .input_data,
        survey_round == .config$survey_round,
        type > 0 | include == 1
      )
    if(nrow(ref_record) > 0) {
      data_files <- data_files |>
        dplyr::filter(tolower(name) %in% ref_record$record_name)
    }
  }

  return(
    list(
      all = all_data_files,
      unique = data_files
    )
  )
}

get_summary_record <- function(.input_data, .type = 'summary_record') {
  .config <- getOption('rcbms.config')
  .config$project[[.input_data]][[.type]]
}

check_input_data <- function(.input_data) {
  .input_data <- .input_data[.input_data %in% c("hp", "bp", "ilq", "cph", "bs")]
  valid_input <- .input_data %in% c("hp", "bp", "ilq", "cph", "bs")
  match_input <- length(which(valid_input))
  if(match_input > 2) {
    .input_data <- .input_data[valid_input]
  } else if (match_input == 0) {
    .input_data <- 'hp'
  }
  return(.input_data)
}


get_file_format <- function(.config, .input_data) {
  if(!is.null(.config$project[[.input_data]][['file_format']])) {
    file_format <- .config$project[[.input_data]][['file_format']]
    file_format <- paste0('\\.(', toupper(file_format), '|', tolower(file_format), ')$')
  } else {
    file_format <- '\\.(TXT|txt)$'
  }
  return(file_format)
}


get_data_path <- function(
  .type,
  .input_data,
  .config = getOption('rcbms.config')
) {
  wd <- .config$working_directory

  if(is.null(wd)) wd <- '.'
  full_path <- file.path(
    wd,
    'src',
    .config$survey_round,
    'data',
    .type,
    .input_data
  )

  sub_directory <- .config$project[[.input_data]]$sub_directory
  if(!is.null(sub_directory)) {
    full_path <- file.path(full_path, sub_directory)
  }

  return(full_path)
}
