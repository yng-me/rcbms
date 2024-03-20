#' Title
#'
#' @param .references
#' @param .config
#'
#' @return
#' @export
#'
#' @examples
read_bp_data <- function(
    .base_path,
    .references = get_config("references"),
    .config = getOption("rcbms.config")) {
  bpq_data_files <- list.files(.base_path, full.names = TRUE, pattern = "\\.(xlsx|xlsm)$") |>
    dplyr::as_tibble() |>
    dplyr::filter(!grepl("[$]", value)) |>
    dplyr::pull(value)

  bpq_data <- list()
  bpq_data_list <- list()
  bpq_data_mode_of_transport <- list()

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

    bpq_data[[i]] <- openxlsx::read.xlsx(path, sheet = "bpq_data") |>
      dplyr::select(variable_name, value) |>
      janitor::clean_names() |>
      dplyr::filter_all(dplyr::any_vars(!is.na(.))) |>
      tidyr::pivot_wider(names_from = variable_name, values_from = value) |>
      add_geo_info(barangay_code) |>
      harmonize_variable(
        .dictionary = .references$data_dictionary,
        .survey_round = .config$survey_round,
        .input_data = "bp",
        .config = .config
      )

    bpq_data_list[[i]] <- openxlsx::read.xlsx(path, sheet = "Lists", startRow = 2) |>
      janitor::clean_names() |>
      dplyr::select(2, 4, 6) |>
      dplyr::filter_all(dplyr::any_vars(!is.na(.))) |>
      dplyr::mutate_all(as.character) |>
      tidyr::pivot_longer(dplyr::everything()) |>
      dplyr::filter(!is.na(value)) |>
      dplyr::rename(variable_name = name) |>
      add_geo_info(barangay_code)

    bpq_data_mode_of_transport[[i]] <- openxlsx::read.xlsx(path, sheet = "Other Transportation", startRow = 3) |>
      janitor::clean_names() |>
      dplyr::select(mode_of_transport = 2, transport_frequency_of_operation = 3) |>
      dplyr::filter_all(dplyr::any_vars(!is.na(.))) |>
      dplyr::mutate(
        mode_of_transport = as.character(mode_of_transport),
        transport_frequency_of_operation = as.integer(transport_frequency_of_operation)
      ) |>
      add_geo_info(barangay_code)
  }

  return(
    list(
      bpq_data = dplyr::bind_rows(bpq_data) |> add_metadata(
        .dictionary = .references$data_dictionary,
        .valueset = .references$valueset,
        .survey_round = .config$survey_round,
        .input_data = "bp"
      ),
      bpq_data_list = dplyr::bind_rows(bpq_data_list),
      bpq_data_mode_of_transport = dplyr::bind_rows(bpq_data_mode_of_transport)
    )
  )
}
