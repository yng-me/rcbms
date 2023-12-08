#' Title
#'
#' @param .config
#'
#' @return
#' @export
#'
#' @examples
load_refs <- function(.config = getOption('rcbms_config')) {

  if(is.null(.config)) {
    stop('Config not found.')
  }

  refs <- list()

  wd <- .config$working_directory
  if(is.null(wd)) wd <- ''

  wd_project <- create_new_folder(paste0(wd, '/src/', .config$cbms_round, '/references'))

  pq_dcf <- paste0(wd_project, '/ref_data_dictionary.parquet')
  pq_vs <- paste0(wd_project, '/ref_valueset.parquet')
  pq_anm <- paste0(wd_project, '/ref_area_name.parquet')

  refs_exist <- file.exists(pq_dcf) & file.exists(pq_vs) & file.exists(pq_anm)

  if(!refs_exist & is_online()) {

    googlesheets4::gs4_deauth()
    refs$data_dictionary <- suppressWarnings(load_data_dictionary(
      .gid = .config$env$DATA_DICTIONARY,
      .cbms_round = .config$cbms_round
    ))
    arrow::write_parquet(refs$data_dictionary, pq_dcf)

    refs$valueset <- suppressWarnings(load_valueset(.config$env$VALUESET))
    arrow::write_parquet(refs$valueset, pq_vs)

    refs$area_name <- suppressWarnings(load_area_name(.config$env$AREA_NAME))
    arrow::write_parquet(refs$area_name, pq_anm)
  }

  if(refs_exist) {
    refs$data_dictionary <- arrow::open_dataset(pq_dcf)
    refs$valueset <- arrow::open_dataset(pq_vs)
    refs$area_name <- arrow::open_dataset(pq_anm)
  }

  return(refs)
}

#' Title
#'
#' @param .gid
#' @param .sheet
#' @param .range
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
fetch_gsheet <- function(.gid, .sheet = NULL, .range = NULL, ...) {
  ss <- paste0("https://docs.google.com/spreadsheets/d/", .gid)
  if(!is.null(.sheet)) .sheet <- as.character(.sheet)

  googlesheets4::read_sheet(
    ss = ss,
    sheet = .sheet,
    range = .range,
    trim_ws = T,
    ...
  ) |> clean_colnames()
}



#' Title
#'
#' @param .data
#' @param .required_cols
#'
#' @return
#' @export
#'
#' @examples
#'
validate_required_cols <- function(.data, .required_cols) {
  required_cols_which <- which(.required_cols %in% names(.data))

  if(length(required_cols_which) < length(.required_cols)) {
    stop('Invalid column names specified.')
  }

  return(.data)
}


#' Title
#'
#' @param .gid
#' @param .required_cols
#' @param .cbms_round
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
load_refs_from_gsheet <- function(.gid, .required_cols, .cbms_round = NULL, ...) {

  range <- paste0(LETTERS[1], ':', LETTERS[length(.required_cols)])
  dd <- fetch_gsheet(.gid, .cbms_round, .range = range, ...)
  return(validate_required_cols(dd, .required_cols))

}



#' Load data dictionary
#'
#' @param .gid
#' @param .cbms_round
#'
#' @return
#' @export
#'
#' @examples
#'
load_data_dictionary <- function(.gid = get_env('DATA_DICTIONARY'), .cbms_round = NULL) {

  required_cols <- c(
    'variable_name',
    'variable_name_new',
    'item',
    'sub_item',
    'label',
    'valueset',
    'type',
    'length',
    'is_included',
    'privacy_level'
  )

  load_refs_from_gsheet(.gid, required_cols, .cbms_round, col_types = 'ccccccciii')
}


#' Load area name
#'
#' @param .gid
#'
#' @return
#' @export
#'
#' @examples
#'
load_area_name <- function(.gid = get_env('AREA_NAME')) {
  required_cols <- c(
    'region',
    'province',
    'city_mun',
    'barangay',
    'geo_new',
    'barangay_geo',
    'income_class',
    'is_huc',
    'class',
    '2015_popn',
    '2020_popn',
    'funding_source'
  )

  df <- load_refs_from_gsheet(.gid, required_cols, col_types = 'ccccccciciii')
}


#' Load valueset
#'
#' @param .gid
#'
#' @return
#' @export
#'
#' @examples
#'
load_valueset <- function(.gid = get_env('VALUESET')) {
  load_refs_from_gsheet(
    .gid,
    .required_cols = c('name', 'value', 'label'),
    col_types = 'ccc'
  )
}
