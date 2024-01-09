save_as_json <- function(.output, .input_data, .config) {

  created_date <- lubridate::now()

  if(!exists('current_area_code')) {
    current_area_code <- ''
  }

  output_json <- list()
  output_json$edit <- .config$mode$edit
  output_json$source <- .config$mode$source
  output_json$input_data <- .input_data
  output_json$created_at <- created_date
  output_json$area_code <- current_area_code
  output_json$inconsistencies <- .output |>
    dplyr::select(
      dplyr::any_of(
        c(
          "id",
          "case_id",
          "validation_id",
          "primary_data_item",
          "line_number"
        )
      )
    )

  jsonlite::write_json(
    output_json,
    set_output_filename(
      ".json",
      .date = created_date,
      .input_data,
      .config
    ),
    pretty = T,
    auto_unbox = T
  )
}

# save_as_excel <- function(.output, .input_data, .config, .detailed) {
#
# }

set_output_filename <- function(.type, .date, .input_data, .config) {

  created_date <- lubridate::now()
  output_path <- create_new_folder(
    join_path(.config$base, 'data', 'json', .input_data)
  )

  stage_label <- c('before', 'after', 'post')
  source_label <- c('dc', 'dp')

  if(exists('current_area_code')) {
    current_area <- paste0('-', current_area_code)
  } else {
    current_area <- ''
  }

  filename <- paste0(
    lubridate::ymd(as.Date(.date)),
    current_area,
    '-validation-',
    .input_data,
    '-',
    source_label[.config$mode$edit],
    '-',
    stage_label[.config$mode$source],
    .type
  )

  paste0(output_path, '/', filename)

}
