#' Generate validation output
#'
#' @param .result
#' @param .config
#' @param .detailed_output
#' @param .include_household_info
#' @param .add_uuid
#' @param .save_as_excel
#' @param .save_as_json
#'
#' @return
#' @export
#'
#' @examples
#'
generate_validation_output <- function(
  .result,
  ...,
  .config = getOption('rcbms.config'),
  .detailed_output = F,
  .include_household_info = F,
  .add_uuid = F,
  .save_as_excel = F,
  .save_as_json = T
) {

  ref <- refs$validation |> dplyr::collect()

  result_names <- names(.result)
  result_names <- result_names[result_names %in% ref$validation_id]

  output <- NULL
  for(i in seq_along(result_names)) {
    result_name <- result_names[i]
    output_temp <- .result[[result_name]] |>
      dplyr::mutate(validation_id = result_name) |>
      dplyr::select(validation_id, case_id, dplyr::any_of('line_number'))

    if(!('line_number' %in% names(output_temp))) {
      output_temp <- output_temp |>
        dplyr::mutate(line_number = NA_character_)
    }

    if(i == 1) {
      output <- output_temp
    } else {
      output <- output |> dplyr::bind_rows(output_temp)
    }
  }

  if(!is.null(output)) {

    opt <- .config$mode$options
    if(!is.null(opt)) {
      if(!is.null(opt$detailed_output)) .detailed_output <- isTRUE(as.logical(opt$detailed_output))
      if(!is.null(opt$include_household_info)) {
        .include_household_info <- isTRUE(as.logical(opt$include_household_info))
      }
      if(!is.null(opt$add_uuid)) .add_uuid <- isTRUE(as.logical(opt$add_uuid))
      if(!is.null(opt$save_as_excel)) .save_as_excel <- isTRUE(as.logical(opt$save_as_excel))
      if(!is.null(opt$save_as_json)) .save_as_json <- isTRUE(as.logical(opt$save_as_json))
    }

    if(.detailed_output) {

      add_length <- .config$project$add_length

      output <- output |>
        dplyr::mutate(barangay_geo = stringr::str_sub(case_id, 1, 9 + add_length)) |>
        dplyr::left_join(
          refs$area_name |>
            transform_area_name() |>
            dplyr::select(barangay_geo, region, province, city_mun, barangay),
          by = 'barangay_geo'
        ) |>
        dplyr::mutate(ean = stringr::str_sub(case_id, 10 + add_length, 15 + add_length)) |>
        dplyr::left_join(ref, by = 'validation_id') |>
        dplyr::select(-barangay_geo)
    }

    if(.add_uuid) {
      id_generated <- uuid::UUIDgenerate(n = nrow(output)) |>
        dplyr::as_tibble() |>
        dplyr::rename(id = value)

      output <- output |> dplyr::bind_cols(id_generated)
    }

    stage_label <- c('before', 'after', 'post')
    source_label <- c('dc', 'dp')

    created_date <- lubridate::now()
    output_path <- create_new_folder(join_path(.config$base, 'data', 'json'))
    output_filename <- paste0(
      '/',
      lubridate::ymd(as.Date(created_date)),
      '-output-validation-',
      source_label[.config$mode$source],
      '-',
      stage_label[.config$mode$stage],
      '.json'
    )

    output_json <- list()
    output_json$stage <- .config$mode$stage
    output_json$source <- .config$mode$source
    output_json$created_at <- created_date
    output_json$inconsistencies <- output

    if(.save_as_json) {

      jsonlite::write_json(
        output_json,
        paste0(output_path, output_filename),
        pretty = T,
        auto_unbox = T
      )
    }

    if(.save_as_excel) {
      print('TODO: save as excel')
    }

  }

  return(output)

}
