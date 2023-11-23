fetch_gsheet <- function(gid, sheet = NULL, range = NULL, ...) {
  ss <- paste0("https://docs.google.com/spreadsheets/d/", gid)
  if(!is.null(sheet)) sheet <- as.character(sheet)

  googlesheets4::read_sheet(
    ss = ss,
    sheet = sheet,
    range = range,
    trim_ws = T,
    ...
  ) |> clean_colnames()
}


validate_gsheet <- function(.data, required_cols) {
  required_cols_which <- which(required_cols %in% names(.data))

  if(length(required_cols_which) < length(required_cols)) {
    stop('Invalid column names defined in the data dictionary.')
  }

  return(.data)
}


load_refs <- function(gid, required_cols, cbms_round = NULL, ...) {

  range <- paste0(LETTERS[1], ':', LETTERS[length(required_cols)])
  dd <- fetch_gsheet(gid, cbms_round, range = range, ...)
  return(validate_gsheet(dd, required_cols))

}

load_area_name <- function(gid) {
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

  load_refs(gid, required_cols, col_types = 'ccccccciciii')
}

load_data_dictionary <- function(gid, cbms_round = NULL) {

  required_cols <- c(
    'variable_name',
    'new_variable_name',
    'item',
    'sub_item',
    'label',
    'valueset',
    'privacy_level',
    'is_included'
  )

  load_refs(gid, required_cols, cbms_round, col_types = 'cccccccicii')
}

load_valueset <- function(gid) {
  required_cols <- c('list_name', 'value', 'label')
  load_refs(gid, required_cols, col_types = 'ccccccciciii')
}
