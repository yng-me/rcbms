#' Load references
#'
#' @param .config
#' @param .reload
#'
#' @return
#' @export
#'
#' @examples
#'

load_references <- function(.config, .wd = NULL, .reload = FALSE) {

  if(is.null(.config)) stop('Config not found.')

  refs <- list()
  is_rcbms_config <- inherits(.config, "rcbms_config")

  if(is_rcbms_config) {

    if(is.null(.wd)) { .wd <- .config$working_directory }
    gid_refs <- gid_references(.wd)
    verbose <- .config$verbose

    if(verbose) {
      cli::cli_h1("Loading References")
    }

    check_if_online <- TRUE
    if(!is.null(.config$check_if_online)) {
      check_if_online <- .config$check_if_online
    }

    ref_reload <- .config$reload_references

  } else {

    gid_refs <- gid_references(.wd)

    if(.config %in% gid_refs$ref | .config %in% gid_refs$ref_short) {
      gid_refs <- gid_refs |>
        dplyr::filter(ref == .config | ref_short == .config)

      ref__i <- gid_refs$ref[1]
      ref_short__i <- gid_refs$ref_short[1]

      verbose <- FALSE
      ref_reload <- .reload
      check_if_online <- .reload

    } else {
      stop("Invalid reference name")
    }

  }

  if (check_if_online) {
    is_online <- is_online()
  } else {
    is_online <- FALSE
  }

  for(i in seq_along(gid_refs$ref)) {

    ref_i <- gid_refs$ref[i]
    ref_short_i <- gid_refs$ref_short[i]
    pq_i <- gid_refs$filename[i]
    gid_i <- gid_refs$gid[i]

    if (length(ref_reload) == 1) {
      ref_reload_i <- ref_reload
    } else {
      ref_reload_i <- ref_reload[[ref_i]]
    }

    if(is_online & (ref_reload_i | !file.exists(pq_i) | .reload)) {

      load_reference_fn <- eval(as.name(paste0("load_", ref_i, "_refs")))
      if(ref_short_i == "anm" | ref_short_i == "anmn") {
        arrow::write_parquet(suppressWarnings(load_reference_fn(gid_i)), pq_i)
      } else {
        jsonlite::write_json(load_reference_fn(gid_i), pq_i, pretty = TRUE)
      }
    }

    if(verbose & !ref_reload_i) {
      cli::cli_alert_info(
        paste0("Loading ", cli::col_br_yellow(ref_i), " reference ", cli::col_br_cyan("✓"))
      )
    }

    if(ref_short_i == "anm" | ref_short_i == "anmn") {
      refs[[ref_i]] <- arrow::read_parquet(pq_i)
    } else {
      refs[[ref_i]] <- jsonlite::fromJSON(pq_i, flatten = T)
    }

    set_class(refs[[ref_i]], paste0("rcbms_", ref_short_i, "_ref"))
  }

  if(is_rcbms_config) {

    refs$script_files <- NULL
    script_files <- lapply(.config$input_data, function(x) {
      get_script_files(x, refs$section, .config = .config)
    }) |> purrr::discard(is.null)

    if(length(script_files) > 0) {
      refs$script_files <- dplyr::bind_rows(script_files)
    }
  }

  if(is_rcbms_config) {
    return(invisible(set_class(refs, "rcbms_refs")))
  } else {
    return(set_class(refs[[ref__i]], paste0("rcbms_", ref_short__i, "_ref")))
  }

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
fetch_gsheet <- function(.gid, ..., .sheet_name = NULL, .range = NULL) {
  googlesheets4::gs4_deauth()

  sheet_pattern <- "^\\d{4}_(bp|hp|cph|bs|ilq|shp)$"
  ss <- paste0("https://docs.google.com/spreadsheets/d/1", .gid)

  ss_names <- googlesheets4::sheet_names(ss) |>
    dplyr::as_tibble() |>
    dplyr::filter(grepl(sheet_pattern, value)) |>
    dplyr::pull(value)

  if(!is.null(.sheet_name)) {
    ss_names <- .sheet_name
  } else {
    if (length(ss_names) == 0) ss_names <- 1
  }

  ss_df <- list()

  for (i in seq_along(ss_names)) {
    ss_name <- ss_names[i]

    ss_df_temp <- googlesheets4::read_sheet(
      ss = ss,
      sheet = ss_name,
      range = .range,
      trim_ws = TRUE,
      ...
    ) |>
      clean_colnames()

    if (grepl(sheet_pattern, ss_name)) {
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

  if (length(required_cols_which) < length(.required_cols)) {
    stop("Invalid column names specified.")
  }

  return(.data)
}


load_refs_from_gsheet <- function(.gid, .required_cols, ..., .sheet_name = NULL, .start_at = 1) {

  range <- paste0(
    LETTERS[.start_at], ":",
    LETTERS[length(.required_cols) + .start_at - 1]
  )
  dd <- fetch_gsheet(.gid, ..., .range = range, .sheet_name = .sheet_name)
  return(validate_required_cols(dd, .required_cols))
}


#' Load data dictionary references
#'
#' @param .transform
#' @param .gid
#'
#' @return
#' @export
#'
#' @examples
load_data_dictionary_refs <- function(.gid, .transform = T) {
  required_cols <- c(
    "variable_name",
    "variable_name_new",
    "item",
    "sub_item",
    "label",
    "valueset",
    "type",
    "length",
    "is_included",
    "privacy_level",
    "is_included_for_portal",
    "is_derived"
  )

  df <- load_refs_from_gsheet(.gid, required_cols, col_types = 'ccccccciiiii') |>
    dplyr::filter(!is.na(variable_name)) |>
    dplyr::mutate(valueset = dplyr::if_else(is.na(valueset), '--', valueset)) |>
    dplyr::mutate(
      variable_name = stringr::str_squish(stringr::str_trim(variable_name)),
      variable_name_new = stringr::str_squish(stringr::str_trim(variable_name_new))
    )

  if(.transform) {
    df <- transform_refs(df)
  }

  return(df)

}

#' Load valueset references
#'
#' @param .gid
#'
#' @return
#' @export
#'
#' @examples
load_valueset_refs <- function(.gid) {

  googlesheets4::gs4_deauth()
  cols <- c('name', 'value', 'label')

  psced <- load_refs_from_gsheet(
    'f0mUpW9KsbyouFJ3AN4k63rMjOguJmvGQCZ0kvy5Ids',
    cols,
    col_types = 'ccc'
  ) |> dplyr::filter(!is.na(name), !is.na(value), !is.na(label))

  psced_vs <- psced |>
    dplyr::mutate(name = 'psced')

  psic <-  load_refs_from_gsheet(
    '8BpYMDCxbydBWD2hkNSRVNMM_2wnsfkPYsT090AjSfg',
    cols,
    col_types = 'ccc'
  ) |> dplyr::filter(!is.na(name), !is.na(value), !is.na(label))

  psoc <- load_refs_from_gsheet(
    'VFFG49u82doxgKLWR_GN_fpiEyl70Si_ikV3qR3pZjQ',
    cols,
    col_types = 'ccc'
  ) |> dplyr::filter(!is.na(name), !is.na(value), !is.na(label))

  load_refs_from_gsheet(.gid, cols, col_types = 'ccc') |>
    dplyr::filter(value != '---') |>
    dplyr::bind_rows(psced) |>
    dplyr::bind_rows(psced_vs) |>
    dplyr::bind_rows(psic) |>
    dplyr::bind_rows(psoc) |>
    dplyr::filter(!is.na(name), !is.na(value), !is.na(label)) |>
    dplyr::mutate(
      name = stringr::str_squish(stringr::str_trim(name)),
      value = stringr::str_squish(stringr::str_trim(value)),
      label = stringr::str_squish(stringr::str_trim(label))
    ) |>
    dplyr::distinct(name, value, label)
}


#' Load validation references
#'
#' @param .gid
#'
#' @return
#' @export
#'
#' @examples
load_validation_refs <- function(.gid) {
  required_cols <- c(
    "validation_id",
    "title",
    "description",
    "primary_data_item",
    "section",
    "priority_level",
    "status",
    "date_introduced"
  )

  df <- load_refs_from_gsheet(.gid, required_cols, col_types = 'ccccccic') |>
    dplyr::filter(status == 1) |>
    dplyr::mutate(type = 1) |>
    dplyr::rename(in_use = status)

  transform_refs(df)

}


#' Load tabulation references
#'
#' @param .gid
#'
#' @return
#' @export
#'
#' @examples
load_tabulation_refs <- function(.gid) {

  required_cols <- c(
    "tabulation_id",
    "tab_name",
    "table_number",
    "title",
    "subtitle",
    "description",
    "is_included",
    "precision",
    "col_decimal_format",
    "col_width_all",
    "col_width_first",
    "col_width_last",
    "row_height_header",
    "row_reset_last"
  )

  df <- load_refs_from_gsheet(
    .gid,
    required_cols,
    col_types = "cccccciiciiici"
  ) |>
    dplyr::rename(
      included = is_included
    ) |>
    dplyr::mutate(
      included = dplyr::if_else(
        included == 1,
        TRUE,
        FALSE
      )
    )

  transform_refs(df)
}


#' Title
#'
#' @param .gid
#'
#' @return
#' @export
#'
#' @examples
load_macrodata_refs <- function(.gid) {

  metadata <- load_refs_from_gsheet(
    .gid,
    .required_cols = c(
      'table_name',
      'variable_name',
      'label',
      'data_type'
    ),
    .sheet_name = "Variables",
    col_types = "ccci"
  ) |>
    dplyr::group_by(table_name) |>
    tidyr::nest(.key = 'meta')

  categories <- load_refs_from_gsheet(
    .gid,
    .required_cols = c(
      'id',
      'module',
      'name',
      'title',
      'subtitle',
      'order',
      'icon',
      'is_available'
    ),
    .sheet_name = "Categories",
    col_types = "iccccici"
  )

  footnotes <- load_refs_from_gsheet(
      .gid,
      .required_cols = c('footnote', 'table_name'),
      .sheet_name = 'Footnotes',
      col_types = "cc"
    ) |>
    dplyr::mutate(
      table_name = stringr::str_split(table_name, ',')
    ) |>
    tidyr::unnest() |>
    dplyr::mutate(
      footnote = stringr::str_trim(footnote),
      table_name = stringr::str_trim(table_name)
    ) |>
    dplyr::group_by(table_name) |>
    tidyr::nest(.key = 'footnote')

  macrodata <- load_refs_from_gsheet(
      .gid,
      .required_cols = c(
        "table_name",
        "category",
        "title",
        "subtitle",
        "description",
        "max_agg"
      ),
      col_types = "ccccci"
    ) |>
    dplyr::left_join(metadata, by = 'table_name') |>
    dplyr::left_join(footnotes, by = 'table_name') |>
    dplyr::left_join(
      categories |>
        dplyr::distinct(id, name) |>
        dplyr::select(category_id = id, category = name),
      by = 'category'
    ) |>
    dplyr::select(
      table_name,
      category_id,
      category,
      dplyr::everything()
    )


  list(
    main = macrodata,
    categories = categories,
    footnotes = footnotes
  )

}


#' Title
#'
#' @param .gid
#'
#' @return
#' @export
#'
#' @examples
load_score_card_refs <- function(.gid) {
  required_cols <- c(
    "variable_name",
    "table_name",
    "type",
    "category",
    "title",
    "subtitle",
    "description",
    "group",
    "order",
    "position",
    "display",
    "value_prefix",
    "value_suffix",
    "unit_of_measure",
    "icon_primary",
    "icon_primary_position",
    "icon_secondary",
    "icon_secondary_position",
    "is_published"
  )

  load_refs_from_gsheet(
    .gid,
    required_cols,
    col_types = "ccicccciiiiccccccci"
  ) |> dplyr::filter(is_published == 1)
}


#' Load record references
#'
#' @param .gid
#
#' @return
#' @export
#'
#' @examples
load_record_refs <- function(.gid) {
  df <- load_refs_from_gsheet(
    .gid,
    .required_cols = c(
      "record_name",
      "short_name",
      "label",
      "uid",
      "order",
      "type",
      "unfiltered",
      "include",
      "expect_equal_rows"
    ),
    col_types = "cccciiiii"
  )

  round <- unique(df$survey_round)

  stats::setNames(
    lapply(round, function(x) {
      df_i <- df |>
        dplyr::filter(survey_round == x) |>
        dplyr::select(-survey_round)

      input <- unique(df_i$input_data)
      z <- stats::setNames(
        lapply(input, function(y) {
          df_i |>
            dplyr::filter(input_data == y) |>
            dplyr::select(-input_data) |>
            dplyr::mutate(
              uid = stringr::str_split(uid, "\\s*?\\|\\s*?")
            )
        }),
        input
      )
    }),
    round
  )
}

load_mpi_spec_refs <- function(.gid) {

  df <- list()

  sheets <- c('cbms', 'interim', 'balisacan')
  for(i in seq_along(sheets)) {

    df[[sheets[i]]] <- load_refs_from_gsheet(
      .gid,
      .required_cols = c(
        "Dimension",
        "Indicator",
        "Variable",
        "Weight",
        "Description"
      ),
      col_types = "cccdc",
      .sheet_name = sheets[i],
    )
  }

  df

}


#' Load record references
#'
#' @param .gid
#'
#' @return
#' @export
#'
#' @examples
load_section_refs <- function(.gid) {
  df <- load_refs_from_gsheet(
    .gid,
    .required_cols = c(
      "section",
      "label",
      "validation_record",
      "validation_script_file",
      "validation_deps"
    ),
    col_types = "ccccc"
  )

  round <- unique(df$survey_round)

  stats::setNames(
    lapply(round, function(x) {
      df_i <- df |>
        dplyr::filter(survey_round == x) |>
        dplyr::select(-survey_round)

      input <- unique(df_i$input_data)
      z <- stats::setNames(
        lapply(input, function(y) {
          df_i |>
            dplyr::filter(input_data == y) |>
            dplyr::select(-input_data) |>
            dplyr::mutate(
              value = tolower(section),
              description = label,
              label = paste0("Section ", section),
              validation = tibble::tibble(
                record = stringr::str_split(validation_record, "\\s*?\\|\\s*?"),
                script_file = stringr::str_split(validation_script_file, "\\s*?\\|\\s*?"),
                deps = stringr::str_split(validation_deps, "\\s*?\\|\\s*?")
              )
            ) |>
            dplyr::select(-dplyr::starts_with("validation_"), -section) |>
            dplyr::mutate(
              included = TRUE,
              builtin_included = TRUE,
              extension_included = FALSE
            )
        }),
        input
      )
    }),
    round
  )
}



#' Title
#'
#' @param .data
#'
#' @return
#' @export
#'
#' @examples
#'
transform_refs <- function(.data) {

  round <- unique(.data$survey_round)

  stats::setNames(
    lapply(round, function(x) {
      df_i <- .data |>
        dplyr::filter(survey_round == x) |>
        dplyr::select(-survey_round)

      input <- unique(df_i$input_data)
      z <- stats::setNames(
        lapply(input, function(y) {
          df_i |>
            dplyr::filter(input_data == y) |>
            dplyr::select(-input_data)
        }),
        input
      )
    }),
    round
  )
}


gid_references <- function(.wd = NULL) {

  if(is.null(.wd)) .wd <- '.'
  wd_base_ref <- create_new_folder(file.path(.wd, 'references'))

  tibble::tibble(
    ref = c(
      "area_name",
      "area_name_new",
      "valueset",
      "validation",
      "tabulation",
      "data_dictionary",
      "record",
      "section",
      "macrodata",
      "score_card"
      # "mpi_spec"
    ),
    ref_short = c(
      "anm",
      "anmn",
      "vs",
      "cv",
      "ts",
      "dcf",
      "rec",
      "sec",
      "macro",
      "sc"
      # "mpi"
    ),
    type = c("parquet", "parquet", rep("json", 8)),
    gid = c(
      "seNZ_CbplwpBrOQiUwIJ2koZONLLFHSicaGFWEKzbrE",
      "1QHcBjkx7BKSsIo8XuDu5x_99EPAhdJk-jczKdtjzXA",
      "eR-sYyLaHMRPRVOkOECTy-iiPQJ-QC8ailyXtNPAA6A",
      "PV5NwM-W3jp8lHmCkE0E84JzeObPYD116bU-xKVVuaY",
      "jfXp-Hao1J4Dkis6E2G_-FZ_mUyA3vGEk0B4aFZvTAE",
      "MU-qx-Va8DpdoZQ5M2I2fexcU4fm-NJAuzr6Os54dMQ",
      "ulDGDAMPjaQomq14ZSyFXSAlk_LQ9RI1xyDhWxLJ_o0",
      "oRBIz5Q0h-wkkrSk2JVdTU5oJINZQTwq4Liti2b1rn0",
      "XC0f3hiCbbd2THEm0cxwR9pI6Eexw-qn_LTm4QmePNg",
      "MohdYBGbYYLVWoL1zmKxAW5i0XNXq-CMsVYDsybQ5G0"
      # "7Ut5Pc1cyh26Yf7eMzKXorLmx0K3OGURMg6z18gH2T8"
    )
  ) |> dplyr::mutate(filename = file.path(wd_base_ref, paste0("ref_", ref, ".", type)))
}
