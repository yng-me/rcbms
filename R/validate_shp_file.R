#' Title
#'
#' @param .dir
#' @param .area_code
#' @param .shp_folder
#' @param .formats
#'
#' @return
#' @export
#'
#' @examples
#'

list_shp_files <- function(
  .dir,
  .area_code = NULL,
  .shp_folder = NULL,
  .formats = 'shp'
) {

  match_file_name <- paste0('^\\d{', nchar(.area_code), '}')
  format <- paste0('\\.(', paste0(.formats, collapse = '|'), ')$')
  file_formats <- paste0(.area_code, '.*\\.(', paste0(.formats, collapse = '|'), ')$')

  path <- .dir

  if(!is.null(.area_code)) {

    path <- file.path(.dir, .area_code)

    if(!is.null(.shp_folder)) {
      path <- file.path(.dir, .area_code, .shp_folder)
    }
  }

  list.files(
      path,
      full.names = T,
      pattern = file_formats,
      ignore.case = T,
      recursive = T
    ) |>
    dplyr::as_tibble() |>
    dplyr::filter(!grepl('deleted', value,  ignore.case = T)) |>
    dplyr::filter(!grepl('[$]', value)) |>
    dplyr::pull(value)
}


#' Title
#'
#' @param .shp_files
#' @param .references
#' @param .config
#'
#' @return
#' @export
#'
#' @examples
#'

import_shp_file <- function(.shp_files, .references, .config) {

  dcf <- .references$data_dictionary[["2024"]][["shp"]]
  if(tolower(.config$mode$stage[1]) > 3) {
    dcf <- dcf |> dplyr::mutate(variable_name = variable_name_new)
  }

  df_list <- list()

  for(i in seq_along(.shp_files)) {

    shp_file <- .shp_files[i]
    shp_layer <- stringr::str_remove_all(basename(shp_file), file_format)

    df_temp <- sf::st_read(shp_file, layer = shp_layer) |>
      dplyr::tibble() |>
      janitor::clean_names() |>
      suppressWarnings()

    df_list[[i]] <- df_temp |>
      harmonize_variable(
        .dictionary = dcf,
        .survey_round = .config$survey_round,
        .input_data = "shp",
        .config = .config
      )
  }

  df_temp <- df_list |>
    dplyr::bind_rows() |>
    dplyr::mutate(
      province_code = stringr::str_sub(cbms_geoid, 1, 3),
      city_mun_code = stringr::str_sub(cbms_geoid, 4, 5),
      barangay_code = stringr::str_sub(cbms_geoid, 6, 8),
      ean = stringr::str_sub(cbms_geoid, 9, 14),
      .before = 2
    ) |>
    dplyr::left_join(
      .references$area_name_new |>
        dplyr::distinct(region_code, province_code),
      by = 'province_code',
      multiple = 'first'
    ) |>
    sort_variable_names('shp', .config) |>
    add_metadata(dcf, .references$valueset) |>
    tibble::tibble() |>
    dplyr::mutate(row_id = dplyr::row_number(), .before = 1)

  pq_folder <- create_new_folder(
    get_data_path("parquet", "shp", .config)
  )

  pq <- file.path(pq_folder, 'shp_data.parquet')

  arrow::write_parquet(df_temp, pq)
  arrow::open_dataset(pq)

}
