#' Title
#'
#' @param .config
#' @param .update_config
#' @param .config_key
#'
#' @return
#' @export
#'
#' @examples
load_references <- function(
  .config = getOption('rcbms_config'),
  .update_config = TRUE,
  .config_key = "references"
) {

  if(is.null(.config)) stop('Config not found.')

  refs <- list()

  wd <- .config$working_directory
  if(is.null(wd)) wd <- ''

  wd_project <- create_new_folder(paste0(wd, '/src/', .config$cbms_round, '/references'))
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
        .gid = .config$env$DATA_DICTIONARY,
        .cbms_round = .config$cbms_round
      )),
      pq_dcf
    )

    arrow::write_parquet(
      suppressWarnings(load_valueset(.config$env$VALUESET)),
      pq_vs
    )

    arrow::write_parquet(
      suppressWarnings(load_area_name(.config$env$AREA_NAME)),
      pq_anm
    )

    arrow::write_parquet(
      suppressWarnings(load_validation_refs(.config$env$VALIDATION)),
      pq_cv
    )
    arrow::write_parquet(
      suppressWarnings(load_tabulation_refs(.config$env$TABULATION)),
      pq_ts
    )

  }

  refs$data_dictionary <- arrow::open_dataset(pq_dcf)
  refs$valueset <- arrow::open_dataset(pq_vs)
  refs$validation <- arrow::open_dataset(pq_cv)
  refs$tabulation <- arrow::open_dataset(pq_ts)
  refs$area_name <- arrow::open_dataset(pq_anm)

  class(refs$data_dictionary) <- c("rcbms_dcf_df", class(refs$data_dictionary))
  class(refs$valueset) <- c("rcbms_vs_df", class(refs$valueset))
  class(refs$validation) <- c("rcbms_cv_df", class(refs$validation))
  class(refs$tabulation) <- c("rcbms_ts_df", class(refs$tabulation))
  class(refs$area_name) <- c("rcbms_anm_df", class(refs$area_name))

  refs$script_files <- NULL

  script_files <- lapply(.config$input_data, get_script_files) |>
    purrr::discard(is.null)

  if(length(script_files) > 0) {
    refs$script_files <- do.call('rbind', script_files) |> dplyr::tibble()
  }


  if(!is.null(.config_key) & .update_config) {
    .config$links$references <- .config_key
    options(rcbms_config = .config)

    assign("config", .config, envir = globalenv())
  }

  assign(.config_key, refs, envir = globalenv())

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
#' @param .sheet
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
load_refs_from_gsheet <- function(.gid, .required_cols, .sheet = NULL, .start_at = 1, ...) {

  range <- paste0(LETTERS[.start_at], ':', LETTERS[length(.required_cols) + .start_at - 1])
  dd <- fetch_gsheet(.gid, .sheet, .range = range, ...)
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

  df <- load_refs_from_gsheet(.gid, required_cols, .cbms_round, col_types = 'ccccccciiiii')

  class(df) <- c('rcbms_dcf', 'rcbms_ref', class(df))

  return(df)
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
      barangay_geo_new = stringr::str_pad(stringr::str_extract(barangay_geo_new, '\\d+'), width = 10, pad = '0'),
      barangay_geo = stringr::str_pad(stringr::str_extract(barangay_geo, '\\d+'), width = 9, pad = '0')
    )

  class(df) <- c('rcbms_anm', 'rcbms_ref', class(df))

  return(df)
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
  df <- load_refs_from_gsheet(
    .gid,
    .required_cols = c('name', 'value', 'label'),
    col_types = 'ccc'
  )
  class(df) <- c('rcbms_vs', 'rcbms_ref', class(df))
  return(df)
}


load_validation_refs <- function(.gid) {
  required_cols <- c(
    'validation_id',
    'title',
    'description',
    'primary_data_item',
    'section',
    'priority_level'
  )
  df <- load_refs_from_gsheet(.gid, required_cols, col_types = 'cccccc')

  attr(df$validation_id, 'label') <- 'Validation ID'
  attr(df$title, 'label') <- 'Title'
  attr(df$description, 'label') <- 'Description'
  attr(df$primary_data_item, 'label') <- 'Primary Data Item'
  attr(df$section, 'label') <- 'Section'
  attr(df$priority_level, 'label') <- 'Priority Level'

  class(df) <- c('rcbms_cv', 'rcbms_ref', class(df))
  return(df)
}

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
  df <- load_refs_from_gsheet(.gid, required_cols, col_types = 'cccccciiciiici')

  class(df) <- c('rcbms_ts', 'rcbms_ref' , class(df))
  return(df)
}

