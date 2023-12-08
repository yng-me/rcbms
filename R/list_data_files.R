list_data_files <- function(.config = getOption('rcbms_config'), .input_data = 'hp') {

  if(is.null(.config)) stop('.config not defined.')

  file_format <- get_file_format(.config, .input_data)
  input_data_path <- get_data_path('raw', .input_data)

  all_data_files <- list.files(
      input_data_path,
      pattern = file_format,
      recursive = T,
      full.names = T
    ) |>
    dplyr::as_tibble() |>
    dplyr::filter(grepl(file_format, value, ignore.case = T)) |>
    dplyr::mutate(file.info(value))

  if(nrow(all_data_files) == 0) {
    stop(paste0('No input data files found for ', .input_data))
  }

  input_file_first <- .config$input_file_first

  if(is.null(input_file_first) | is.na(input_file_first)) {
    input_file_first <- ''
  }

  data_files <- dplyr::as_tibble(basename(all_data_files$value)) %>%
    dplyr::filter(grepl(file_format, value, ignore.case = T)) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      name = tolower(stringr::str_remove(value, file_format)),
      n = seq(1:dplyr::n()),
      n = dplyr::if_else(grepl(paste0('^', input_file_first), name), 0L, n)
    ) %>%
    dplyr::arrange(n)

  return(
    list(
      all = all_data_files,
      unique = data_files
    )
  )
}

check_input_data <- function(.input_data = 'hp') {
  .input_data <- .input_data[.input_data %in% c('hp', 'bp')]
  valid_input <- .input_data %in% c('hp', 'bp')
  match_input <- length(which(valid_input))
  if(match_input > 2) {
    .input_data <- .input_data[valid_input]
  } else if (match_input == 0) {
    .input_data <- 'hp'
  }
  return(.input_data)
}

get_file_format <- function(.config, .input_data) {
  if(!is.null(.config$file_format[[.input_data]])) {
    file_format <- .config$file_format[[.input_data]]
    file_format <- paste0('\\.(', toupper(file_format), '|', tolower(file_format), ')$')
  } else {
    file_format <- '\\.(TXT|txt)$'
  }
  return(file_format)
}

get_data_path <- function(.type, .input_data = 'hp', .config = getOption('rcbms_config')) {
  wd <- .config$working_directory
  if(is.null(wd)) wd <- '.'
  full_path <- file.path(
    wd,
    'src',
    .config$cbms_round,
    'data',
    .type,
    .input_data
  )
  return(full_path)
}
