#' Title
#'
#' @param .dictionary
#' @param .config
#'
#' @return
#' @export
#'
#' @examples
#'

read_bp_data <- function(.dictionary, .config) {

  bp_base_path <- .config$project$bp$directory
  if(is.null(bp_base_path)) {
    bp_base_path <- file.path(.config$base, "data", "raw", "bp")
  }

  bpq_data_files <- list.files(bp_base_path, full.names = T, pattern = "\\.(xlsx|xlsm)$") |>
    dplyr::as_tibble() |>
    dplyr::filter(!grepl('[$]', value)) |>
    dplyr::filter(!grepl('Form6', value)) |>
    dplyr::filter(grepl('^\\d{10}_Form5_', basename(value))) |>
    dplyr::pull(value)

  bpq_data <- list()
  bpq_data_list <- list()

  add_geo_info <- function(.data, .code) {
    .data |>
      dplyr::mutate(
        barangay_geo = .code,
        region_code = stringr::str_sub(.code, 1, 2),
        province_code = stringr::str_sub(.code, 3, 5),
        city_mun_code = stringr::str_sub(.code, 6, 7),
        barangay_code = stringr::str_sub(.code, 8, 10),
        .before = 1
      )
  }

  for (i in seq_along(bpq_data_files)) {

    path <- bpq_data_files[i]

    barangay_code <- basename(path) |>
      stringr::str_extract(pattern = "^\\d{10}")

    if(.config$verbose) {
      cli::cli_alert_info(
        paste0("Importing ", cli::col_br_yellow(basename(path)), " record ", cli::col_br_cyan("✓"))
      )
    }

    if(.config$progress) {
      cli::cli_text(paste0('Importing ', basename(path)))
    }

    bpq_data[[i]] <- openxlsx::read.xlsx(path, sheet = "bpq_data") |>
      janitor::clean_names() |>
      dplyr::select(variable_name, value) |>
      dplyr::mutate(
        variable_name = stringr::str_squish(stringr::str_trim(variable_name)),
        value = stringr::str_squish(stringr::str_trim(value))
      ) |>
      convert_to_na() |>
      convert_to_na('NA') |>
      dplyr::filter_all(dplyr::any_vars(!is.na(.))) |>
      tidyr::pivot_wider(names_from = variable_name, values_from = value) |>
      add_geo_info(barangay_code) |>
      harmonize_variable(
        .dictionary = .dictionary,
        .survey_round = .config$survey_round,
        .input_data = "bp",
        .config = .config
      ) |>
      suppressWarnings()

    bpq_data_list[[i]] <- openxlsx::read.xlsx(path, sheet = "Lists", startRow = 2) |>
      janitor::clean_names() |>
      dplyr::select(2, 4) |>
      dplyr::filter_all(dplyr::any_vars(!is.na(.))) |>
      dplyr::mutate_all(as.character) |>
      tidyr::pivot_longer(dplyr::everything()) |>
      dplyr::filter(!is.na(value)) |>
      dplyr::rename(variable_name = name) |>
      add_geo_info(barangay_code) |>
      suppressWarnings()

  }

  return(
    list(
      bpq_data = dplyr::bind_rows(bpq_data),
      bpq_data_list = dplyr::bind_rows(bpq_data_list)
    )
  )
}
