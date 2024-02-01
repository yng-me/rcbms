#' Title
#'
#' @param .path
#' @param .subfolders
#'
#' @return
#' @export
#'
#' @examples
#'

tidy_geojson <- function(
  .path,
  .subfolders = c("region", "province", "city-mun", "barangay")
) {

  to_camel_case <- function(x) {
    y <- x |>
      stringr::str_to_lower() |>
      stringr::str_split('-') |>
      purrr::map(~ .x |> purrr::map(~ stringr::str_to_title(.x)) |> stringr::str_c(collapse = ''))
    paste0(tolower(stringr::str_sub(y, 1, 1)), stringr::str_sub(y, 2, -1))
  }

  geo_path <- paste0(.path, .subfolders)
  geo_json_files <- list.files(geo_path, full.names = T, pattern = "\\.json$")

  geo_json_list <- list()

  for(i in seq_along(geo_json_files)) {

    geo_file <- geo_json_files[i]
    geo_json_file <- str_remove(basename(geo_file), "\\.json$")
    geo_json_name <- str_extract(geo_json_file, "\\d+")

    agg_labels <- stringr::str_extract(geo_file, .subfolders)
    agg_labels <- agg_labels[!is.na(agg_labels)]
    agg_level_var <- paste0(to_camel_case(agg_labels[1]), "Code")

    tbl <- geo_file |> jsonlite::read_json()

    geo_json_list[[geo_json_file]] <- tbl |>
      purrr::pluck("features") |>
      purrr::map(\(x) {
        x <- x |>
          purrr::pluck("properties") |>
          dplyr::as_tibble()

        if(agg_level_var %in% names(x)) {
          x |> dplyr::select(area_code = !!as.name(agg_level_var))
        } else {
          x |> dplyr::transmute(area_code = '')
        }

      }) |>
      dplyr::bind_rows() |>
      dplyr::bind_cols(
        tbl |>
          purrr::pluck('features') |>
          purrr::map(\(x) {

            geometry <- x |>
              purrr::pluck("geometry")

            if(!is.null(geometry)) {

              type <- geometry |>
                purrr::pluck("type")

              if(length(type) == 0) {
                type <- ""
              } else {
                type <- type |> purrr::pluck(1)
              }

              type <- type |>
                dplyr::as_tibble() |>
                dplyr::rename(type = value)

              coordinates <- geometry |>
                purrr::pluck("coordinates")

              if(length(coordinates) > 0) {
                coordinates <- as.character(jsonlite::toJSON(coordinates))
              } else {
                coordinates <- ""
              }

              coordinates |>
                dplyr::as_tibble() |>
                dplyr::rename(coordinates = value) |>
                dplyr::bind_cols(type)
            }

          }) |>
          dplyr::bind_rows()
      ) |>
      dplyr::mutate(
        collection = tbl |> pluck('type') |> pluck(1),
        group = geo_json_name,
        level = as.integer(which(.subfolders == agg_labels[1]))
      )
  }

  dplyr::bind_rows(geo_json_list)

}
