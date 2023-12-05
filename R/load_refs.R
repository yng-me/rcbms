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
validate_gsheet <- function(.data, .required_cols) {
  required_cols_which <- which(.required_cols %in% names(.data))

  if(length(required_cols_which) < length(.required_cols)) {
    stop('Invalid column names defined in the data dictionary.')
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
load_refs <- function(.gid, .required_cols, .cbms_round = NULL, ...) {

  range <- paste0(LETTERS[1], ':', LETTERS[length(.required_cols)])
  dd <- fetch_gsheet(.gid, .cbms_round, .range = range, ...)
  return(validate_gsheet(dd, .required_cols))

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
load_data_dictionary <- function(.gid, .cbms_round = NULL) {

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

  load_refs(.gid, required_cols, .cbms_round, col_types = 'ccccccciii')
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
load_area_name <- function(.gid) {
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

  load_refs(.gid, required_cols, col_types = 'ccccccciciii')
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
load_valueset <- function(.gid) {
  required_cols <- c('name', 'value', 'label')
  load_refs(.gid, required_cols, col_types = 'ccccccciciii')
}
