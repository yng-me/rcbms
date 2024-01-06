#' Load references
#'
#' @param .config
#' @param .update_config
#' @param .config_key
#'
#' @return
#' @export
#'
#' @examples
#'

load_references <- function(
  .config = getOption('rcbms.config'),
  .update_config = TRUE,
  .config_key = "references"
) {

  if(is.null(.config)) stop('Config not found.')

  refs <- list()

  wd <- .config$working_directory
  if(is.null(wd)) wd <- ''

  wd_project <- create_new_folder(
    paste0(wd, '/src/', .config$cbms_round, '/references')
  )
  wd_base_ref <- create_new_folder(paste0(wd, '/references'))

  pq_dcf <- paste0(wd_project, '/ref_data_dictionary.parquet')
  pq_cv <- paste0(wd_project, '/ref_validation.parquet')
  pq_ts <- paste0(wd_project, '/ref_tabulation.parquet')

  pq_vs <- paste0(wd_base_ref, '/ref_valueset.parquet')
  pq_anm <- paste0(wd_base_ref, '/ref_area_name.parquet')

  refs_exist <- file.exists(pq_dcf) &
    file.exists(pq_vs) &
    file.exists(pq_anm) &
    file.exists(pq_cv) &
    file.exists(pq_ts)

  if((!refs_exist | .config$reload_references) & is_online()) {

    googlesheets4::gs4_deauth()

    arrow::write_parquet(
      suppressWarnings(load_data_dictionary(
        .gid = .config$env$data_dictionary,
        .cbms_round = .config$cbms_round
      )),
      pq_dcf
    )

    gid <- .config$references

    arrow::write_parquet(
      suppressWarnings(load_valueset(gid$valueset)),
      pq_vs
    )

    arrow::write_parquet(
      suppressWarnings(load_area_name(gid$area_name)),
      pq_anm
    )

    arrow::write_parquet(
      suppressWarnings(load_validation_refs(gid$validation)),
      pq_cv
    )
    arrow::write_parquet(
      suppressWarnings(load_tabulation_refs(gid$tabulation)),
      pq_ts
    )

  }

  refs$data_dictionary <- arrow::open_dataset(pq_dcf) |>
    set_class("rcbms_dcf_ref")

  refs$valueset <- arrow::open_dataset(pq_vs) |>
    set_class("rcbms_vs_ref")

  refs$validation <- arrow::open_dataset(pq_cv) |>
    set_class("rcbms_cv_ref")

  refs$tabulation <- arrow::open_dataset(pq_ts) |>
    set_class("rcbms_ts_ref")

  refs$area_name <- arrow::open_dataset(pq_anm) |>
    set_class("rcbms_anm_ref")

  refs$script_files <- NULL

  script_files <- lapply(.config$input_data, get_script_files) |>
    purrr::discard(is.null)

  if(length(script_files) > 0) {
    refs$script_files <- do.call('rbind', script_files) |> dplyr::tibble()
  }

  envir <- as.environment(1)
  if(!is.null(.config_key) & .update_config) {
    .config$links$references <- .config_key
    options(rcbms.config = .config)

    assign("config", .config, envir = envir)
  }

  refs <- set_class(refs, "rcbms_refs")
  assign(.config_key, refs, envir = envir)

  return(invisible(refs))

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
  ss <- paste0("https://docs.google.com/spreadsheets/d/1", .gid)
  if(!is.null(.sheet)) .sheet <- as.character(.sheet)

  googlesheets4::read_sheet(
    ss = ss,
    sheet = .sheet,
    range = .range,
    trim_ws = T,
    ...
  ) |> clean_colnames()
}


validate_required_cols <- function(.data, .required_cols) {
  required_cols_which <- which(.required_cols %in% names(.data))

  if(length(required_cols_which) < length(.required_cols)) {
    stop('Invalid column names specified.')
  }

  return(.data)
}


load_refs_from_gsheet <- function(
  .gid,
  .required_cols,
  .sheet = NULL,
  .start_at = 1,
  ...
) {

  range <- paste0(
    LETTERS[.start_at], ':',
    LETTERS[length(.required_cols) + .start_at - 1]
  )
  dd <- fetch_gsheet(.gid, .sheet, .range = range, ...)
  return(validate_required_cols(dd, .required_cols))

}


#' Load data dictionary references
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
    'privacy_level',
    'is_included_for_portal',
    'is_derived'
  )

  df <- load_refs_from_gsheet(
    .gid,
    required_cols,
    .cbms_round,
    col_types = 'ccccccciiiii'
  )

  set_class(df, "rcbms_dcf_ref")
}


#' Load area name references
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
    # 'region',
    'province',
    'city_mun',
    'barangay',
    'barangay_geo_new',
    'barangay_geo',
    'income_class',
    'is_huc',
    'class',
    '2015_popn',
    '2020_popn',
    'funding_source'
  )

  df <- load_refs_from_gsheet(
      .gid,
      .required_cols = required_cols,
      .start_at = 2,
      col_types = 'ccciiciciii'
    ) |>
    dplyr::mutate(
      barangay_geo_new = stringr::str_pad(
        stringr::str_extract(barangay_geo_new, '\\d+'),
        width = 10,
        pad = '0'
      ),
      barangay_geo = stringr::str_pad(
        stringr::str_extract(barangay_geo, '\\d+'),
        width = 9,
        pad = '0'
      )
    )

  set_class(df, "rcbms_anm_ref")

}

#' Load valueset references
#'
#' @param .gid
#'
#' @return
#' @export
#'
#' @examples
#'
load_valueset <- function(.gid) {
  df <- load_refs_from_gsheet(
    .gid,
    .required_cols = c('name', 'value', 'label'),
    col_types = 'ccc'
  )
  set_class(df, "rcbms_vs_ref")
}


#' Load validation references
#'
#' @param .gid
#'
#' @return
#' @export
#'
#' @examples
#'

load_validation_refs <- function(.gid) {
  required_cols <- c(
    'validation_id',
    'title',
    'description',
    'primary_data_item',
    'section',
    'priority_level'
  )
  df <- load_refs_from_gsheet(
    .gid,
    required_cols,
    col_types = 'cccccc'
  )

  attr(df$validation_id, 'label') <- 'Validation ID'
  attr(df$title, 'label') <- 'Title'
  attr(df$description, 'label') <- 'Description'
  attr(df$primary_data_item, 'label') <- 'Primary Data Item'
  attr(df$section, 'label') <- 'Section'
  attr(df$priority_level, 'label') <- 'Priority Level'

  set_class(df, "rcbms_cv_ref")
}


#' Load tabulation references
#'
#' @param .gid
#'
#' @return
#' @export
#'
#' @examples
#'
load_tabulation_refs <- function(.gid) {
  required_cols <- c(
    'tabulation_id',
    'tab_name',
    'table_number',
    'title',
    'subtitle',
    'description',
    'is_included',
    'precision',
    'col_decimal_format',
    'col_width_all',
    'col_width_first',
    'col_width_last',
    'row_height_header',
    'row_reset_last'
  )
  df <- load_refs_from_gsheet(
    .gid,
    required_cols,
    col_types = 'cccccciiciiici'
  )

  set_class(df, "rcbms_ts_ref")
}

