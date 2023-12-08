list_data_files <- function(.config = getOption('rcbms_config')) {

  if(is.null(.config)) stop('.config not defined.')

  file_format <- '\\.(TXT|txt)$'
  input_data <- .config$input_data

  input_data_path <- get_data_path('raw')

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
    stop(paste0('No input data files found for ', input_data))
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

get_data_path <- function(.type, .config = getOption('rcbms_config')) {
  wd <- .config$working_directory
  if(is.null(wd)) wd <- '.'
  full_path <- file.path(
    wd,
    'src',
    .config$cbms_round,
    'data',
    .type,
    .config$input_data
  )
  return(full_path)
}
