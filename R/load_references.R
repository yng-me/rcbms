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

  gid_area_name <- "seNZ_CbplwpBrOQiUwIJ2koZONLLFHSicaGFWEKzbrE"
  gid_valueset <- "eR-sYyLaHMRPRVOkOECTy-iiPQJ-QC8ailyXtNPAA6A"
  gid_validation <- "PV5NwM-W3jp8lHmCkE0E84JzeObPYD116bU-xKVVuaY"
  gid_tabulation <- "jfXp-Hao1J4Dkis6E2G_-FZ_mUyA3vGEk0B4aFZvTAE"
  gid_data_dictionary <- "MU-qx-Va8DpdoZQ5M2I2fexcU4fm-NJAuzr6Os54dMQ"

  if(is.null(.config)) stop('Config not found.')

  if(.config$verbose) {
    cli::cli_h1("Loading References")
  }

  refs <- list()

  wd <- .config$working_directory
  if(is.null(wd)) wd <- ''
  wd_base_ref <- create_new_folder(paste0(wd, '/references'))

  pq_dcf <- paste0(wd_base_ref, '/ref_data_dictionary.parquet')
  pq_cv <- paste0(wd_base_ref, '/ref_validation.parquet')
  pq_ts <- paste0(wd_base_ref, '/ref_tabulation.parquet')
  pq_vs <- paste0(wd_base_ref, '/ref_valueset.parquet')
  pq_anm <- paste0(wd_base_ref, '/ref_area_name.parquet')

  refs_exist <- file.exists(pq_dcf) &&
    file.exists(pq_vs) &&
    file.exists(pq_anm) &&
    file.exists(pq_cv) &&
    file.exists(pq_ts)

  reload_ref <- .config$reload_references

  if(length(reload_ref) == 1) {
    reload_dcf <- reload_ref
    reload_vs <- reload_ref
    reload_anm <- reload_ref
    reload_cv <- reload_ref
    reload_ts <- reload_ref
  } else {
    reload_dcf <- reload_ref$data_dictionary
    reload_vs <- reload_ref$valueset
    reload_anm <- reload_ref$area_name
    reload_cv <- reload_ref$validation
    reload_ts <- reload_ref$tabulation
    reload_ref <- reload_dcf || reload_vs || reload_anm || reload_cv || reload_ts
  }

  ref_list <- c("data_dictionary", "valueset", "area_name", "validation", "tabulation")
  ref_list_short <- c("dcf", "vs", "anm", "cv", "ts")

  if((!refs_exist || reload_ref) && is_online()) {

    googlesheets4::gs4_deauth()
    gid <- .config$references

    for(i in seq_along(ref_list)) {

      ref <- ref_list[[i]]
      ref_short <- ref_list_short[[i]]

      if(eval(parse(text = paste0("reload_", ref_short)))) {
        load_reference_fn <- eval(as.name(paste0("load_", ref, "_refs")))
        arrow::write_parquet(
          suppressWarnings(load_reference_fn(eval(as.name(paste0("gid_", ref))))),
          eval(parse(text = paste0("pq_", ref_short)))
        )
      }
    }
  }

  for(i in seq_along(ref_list)) {

    ref <- ref_list[[i]]
    ref_short <- ref_list_short[[i]]

    if(.config$verbose) {
      if(!eval(parse(text = paste0("reload_", ref_short)))) {
        cli::cli_alert_info(
          paste0("Loading ", cli::col_br_yellow(ref), " reference ", cli::col_br_cyan("✓"))
        )
      }
    }

    refs[[ref]] <- arrow::open_dataset(eval(parse(text = paste0("pq_", ref_short))))

    if(!("survey_round" %in% names(refs[[ref]])) && "cbms_round" %in% names(refs[[ref]])) {
      refs[[ref]] <- refs[[ref]] |>
        dplyr::rename(survey_round = cbms_round)
    }

    set_class(refs[[ref]], paste0("rcbms_", ref_short, "_ref"))

  }

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
#' @param ...
#' @param .range
#'
#' @return
#' @export
#'
#' @examples
#'
fetch_gsheet <- function(.gid, ..., .range = NULL) {
  sheet_pattern <- "^\\d{4}_[hb]p$"
  ss <- paste0("https://docs.google.com/spreadsheets/d/1", .gid)

  ss_names <- googlesheets4::sheet_names(ss) |>
    dplyr::as_tibble() |>
    dplyr::filter(grepl(sheet_pattern, value)) |>
    dplyr::pull(value)

  if(length(ss_names) == 0) ss_names <- 1

  ss_df <- list()

  for(i in seq_along(ss_names)) {

    ss_name <- ss_names[i]

    ss_df_temp <- googlesheets4::read_sheet(
        ss = ss,
        sheet = ss_name,
        range = .range,
        trim_ws = TRUE,
        ...
      ) |>
      clean_colnames()

    if(grepl(sheet_pattern, ss_name)) {
      ss_df_temp <- ss_df_temp |>
        dplyr::mutate(
          survey_round = as.integer(stringr::str_sub(ss_name, 1, 4)),
          input_data = stringr::str_sub(ss_name, 6, -1)
        )

      attr(ss_df_temp$survey_round, "label") <- "Survey Round"
      attr(ss_df_temp$input_data, "label") <- "Input Data"
    }

    ss_df[[i]] <- ss_df_temp
  }

  do.call("rbind", ss_df)

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
  ...,
  .start_at = 1
) {

  range <- paste0(
    LETTERS[.start_at], ':',
    LETTERS[length(.required_cols) + .start_at - 1]
  )
  dd <- fetch_gsheet(.gid, ..., .range = range)
  return(validate_required_cols(dd, .required_cols))

}


#' Load data dictionary references
#'
#' @param .gid
#' @param .survey_round
#'
#' @return
#' @export
#'
#' @examples
#'

load_data_dictionary_refs <- function(.gid) {

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
    .required_cols = required_cols,
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
load_area_name_refs <- function(.gid) {

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
      col_types = 'ccciiciciii',
      .start_at = 2
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
load_valueset_refs <- function(.gid) {
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
    'priority_level',
    'status',
    'date_introduced'
  )
  df <- load_refs_from_gsheet(
    .gid,
    required_cols,
    col_types = 'cccccccc'
  )

  attr(df$validation_id, 'label') <- 'Validation ID'
  attr(df$title, 'label') <- 'Title'
  attr(df$description, 'label') <- 'Description'
  attr(df$primary_data_item, 'label') <- 'Primary Data Item'
  attr(df$section, 'label') <- 'Section'
  attr(df$priority_level, 'label') <- 'Priority Level'
  attr(df$status, 'label') <- 'Status'
  attr(df$date_introduced, 'label') <- 'Date Introduced'

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

