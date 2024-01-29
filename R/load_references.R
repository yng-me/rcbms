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

  if(.config$verbose) {
    cli::cli_h1("Loading References")
  }

  gid <- list(
    area_name = "seNZ_CbplwpBrOQiUwIJ2koZONLLFHSicaGFWEKzbrE",
    valueset = "eR-sYyLaHMRPRVOkOECTy-iiPQJ-QC8ailyXtNPAA6A",
    validation = "PV5NwM-W3jp8lHmCkE0E84JzeObPYD116bU-xKVVuaY",
    tabulation = "jfXp-Hao1J4Dkis6E2G_-FZ_mUyA3vGEk0B4aFZvTAE",
    data_dictionary = "MU-qx-Va8DpdoZQ5M2I2fexcU4fm-NJAuzr6Os54dMQ",
    macrodata = "XC0f3hiCbbd2THEm0cxwR9pI6Eexw-qn_LTm4QmePNg",
    score_card = "MohdYBGbYYLVWoL1zmKxAW5i0XNXq-CMsVYDsybQ5G0",
    record = "ulDGDAMPjaQomq14ZSyFXSAlk_LQ9RI1xyDhWxLJ_o0"
  )

  refs <- list()
  ref_list <- c("data_dictionary", "valueset", "area_name", "validation", "tabulation", "macrodata", "score_card", "record")
  ref_list_short <- c("dcf", "vs", "anm", "cv", "ts", "macro", "sc", "rec")

  wd <- .config$working_directory
  if(is.null(wd)) wd <- ''
  wd_base_ref <- create_new_folder(paste0(wd, '/references'))

  pq <- stats::setNames(
    lapply(ref_list, function(x) {
      if(x == "record") {
        paste0(wd_base_ref, '/ref_', x, '.xlsx')
      } else {
        paste0(wd_base_ref, '/ref_', x, '.parquet')
      }
    }),
    ref_list
  )

  ref_reload <- .config$reload_references

  if(is.null(ref_reload$record)) ref_reload$record <- TRUE
  if(is.null(ref_reload$macrodata)) ref_reload$macrodata <- TRUE
  if(is.null(ref_reload$score_card)) ref_reload$score_card <- TRUE

  for(i in seq_along(ref_list)) {

    ref_i <- ref_list[[i]]
    ref_short_i <- ref_list_short[[i]]
    pq_i <- pq[[ref_i]]
    gid_i <- gid[[ref_i]]

    if(length(ref_reload) == 1) {
      ref_reload_i <- ref_reload
    } else {
      ref_reload_i <- ref_reload[[ref_i]]
    }

    if(is_online() & (ref_reload_i | !file.exists(pq_i))) {

      load_reference_fn <- eval(as.name(paste0("load_", ref_i, "_refs")))
      if(ref_short_i == "rec") {
        openxlsx::write.xlsx(load_reference_fn(gid_i), pq_i)
      } else {
        arrow::write_parquet(suppressWarnings(load_reference_fn(gid_i)), pq_i)
      }
    }

    if(.config$verbose && !ref_reload_i) {
      cli::cli_alert_info(
        paste0("Loading ", cli::col_br_yellow(ref_i), " reference ", cli::col_br_cyan("✓"))
      )
    }

    if(ref_short_i == "rec") {
      refs[[ref_i]] <- openxlsx::read.xlsx(pq_i)
    } else {
      refs[[ref_i]] <- arrow::read_parquet(pq_i)
    }

    if(!("survey_round" %in% names(refs[[ref_i]])) && "cbms_round" %in% names(refs[[ref_i]])) {
      refs[[ref_i]] <- refs[[ref_i]] |>
        dplyr::rename(survey_round = cbms_round)
    }

    set_class(refs[[ref_i]], paste0("rcbms_", ref_short_i, "_ref"))

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

  googlesheets4::gs4_deauth()

  sheet_pattern <- "^\\d{4}_(bp|hp|cph|bs|ilq)$"
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

  load_refs_from_gsheet(.gid, .required_cols = required_cols, col_types = 'ccccccciiiii') |>
    dplyr::filter(!is.na(variable_name))
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

  load_refs_from_gsheet(
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
  load_refs_from_gsheet(
    .gid,
    .required_cols = c('name', 'value', 'label'),
    col_types = 'ccc'
  )
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

  df

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

  load_refs_from_gsheet(
    .gid,
    required_cols,
    col_types = 'cccccciiciiici'
  )
}


load_macrodata_refs <- function(.gid) {
  required_cols <- c(
    'table_name',
    'category',
    'title',
    'subtitle',
    'description'
  )

  load_refs_from_gsheet(
    .gid,
    required_cols,
    col_types = 'ccccc'
  )
}



load_score_card_refs <- function(.gid) {
  required_cols <- c(
    'variable_name',
    'table_name',
    'type',
    'category',
    'title',
    'subtitle',
    'description',
    'group',
    'order',
    'position',
    'display',
    'value_prefix',
    'value_suffix',
    'unit_of_measure',
    'icon_primary',
    'icon_primary_position',
    'icon_secondary',
    'icon_secondary_position',
    'is_published'
  )

  load_refs_from_gsheet(
    .gid,
    required_cols,
    col_types = 'ccicccciiiiccccccci'
  )
}





#' Load record references
#'
#' @param .gid
#'
#' @return
#' @export
#'
#' @examples
#'
load_record_refs <- function(.gid) {
  load_refs_from_gsheet(
    .gid,
    .required_cols = c(
      "record_name",
      "label",
      "order",
      "type",
      "unfiltered",
      "include",
      "expect_equal_rows"
    ),
    col_types = 'cciiiii'
  )
}
