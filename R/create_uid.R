#' Create case ID
#'
#' @param .data
#' @param .filter_completed
#' @param .add_length
#'
#' @return
#' @export
#'
#' @examples
create_case_id <- function(.data, .filter_completed = T, .add_length = 0) {

  .data <- .data |> collect()

  if(!('case_id' %in% names(.data))) {

    .data <- .data |>
      collect() |>
      mutate(
        case_id = paste0(
          str_pad(region_code, width = 2, pad = '0'),
          str_pad(province_code, width = 2 + .add_length, pad = '0'),
          str_pad(city_mun_code, width = 2, pad = '0'),
          str_pad(barangay_code, width = 3, pad = '0'),
          str_pad(ean, width = 6, pad = '0'),
          str_pad(bsn, width = 4 + .add_length, pad = '0'),
          str_pad(husn, width = 4 + .add_length, pad = '0'),
          str_pad(hsn, width = 4 + .add_length, pad = '0')
        )
      ) |>
      select(case_id, everything())

  }

  config <- getOption('rcbms_config')

  if(!is.null(config$completed_cases) & .filter_completed) {
    .data <- .data |>
      filter(case_id %in% config$completed_cases)
  }

  return(.data)

}

#' Create line number ID
#'
#' @param .data
#' @param .join_with
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
create_line_number_id <- function(.data, .join_with = NULL, ...) {
  if(!('case_id' %in% names(.data))) {
    .data <- .data |>
      create_case_id(...)
  }
  df <- .data |>
    mutate(line_number_id = if_else(
      !is.na(line_number),
      paste0(case_id, str_pad(as.integer(line_number), width = 2, pad = '0')),
      NA_character_)
    )

  if(!is.null(.join_with)) {
    df <- df |>
      left_join(.join_with, by = 'line_number_id')
  }
  return(df |> select(case_id, line_number_id, everything()))
}


#' Create barangay geo ID
#'
#' @param .data
#' @param .add_length
#'
#' @return
#' @export
#'
#' @examples
create_barangay_geo <- function(.data, .add_length = 0) {
  .data |>
    mutate(
      barangay_geo = paste0(
        str_pad(as.character(region_code), width = 2, pad = '0'),
        str_pad(as.character(province_code), width = 2 + .add_length, pad = '0'),
        str_pad(as.character(city_mun_code), width = 2, pad = '0'),
        str_pad(as.character(barangay_code), width = 3, pad = '0')
      )
    )
}


#' Create unique ID sequentially
#'
#' @param .format
#'
#' @return
#' @export
#'
#' @examples
create_uid <- function(.format) {
  concat <- c()
  for(i in 1:nrow(.format)) {
    append <- ''
    if(i < nrow(.format)) append <- ', '
    concat <- paste0(
      concat,
      paste0(
        "sprintf('%0",
        .format$digit[i],
        "d', as.integer(",
        .format$var_name[i],
        "))",
        append
      )
    )
  }

  return(paste0("paste0(", concat, ")"))

}
