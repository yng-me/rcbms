#' Title
#'
#' @param ...
#' @param code_ref
#' @param input_data
#' @param is_char
#' @param ordered
#'
#' @return
#' @export
#'
#' @examples
convert_to_factor <- function(
  ..., code_ref = NULL,
  input_data = 'hp',
  is_char = F,
  ordered = F
) {

  if(is.null(code_ref) & !exists('refs')) {
    stop('References code does not exist.')
  }

  if(!is.null(code_ref)) {

    if(exists('refs')) {
      code_refs <- eval(as.name('refs'))
      refs_vs <- code_refs[[input_data]]$valueset |>
        filter(list_name == code_ref)
    }

    if(is_char == T) {
      code <- refs_vs |>
        mutate(level = str_trim(value))
    } else {
      code <- refs_vs |>
        mutate(level = as.integer(value))
    }

    factor(..., levels = code$level, labels = code$label, ordered = ordered)

  } else {
    factor(..., ordered = ordered)
  }

}

#' Title
#'
#' @param .data
#' @param .convert_value
#' @param .pattern
#'
#' @return
#' @export
#'
#' @examples
convert_to_na <- function(.data, .convert_value = '', .pattern = NULL) {

  if(!is.null(.pattern)) {
    .data <- .data |>
      dplyr::mutate_if(
        is.character,
        ~ stringr::str_replace_all(., .pattern, .convert_value)
      )
  }

  suppressWarnings(
    .data <- .data |>
      dplyr::mutate_if(
        is.character,
        ~ dplyr::if_else(. == .convert_value, NA_character_, .)
      )
  )

  return(.data)
}


#' Title
#'
#' @param .from
#' @param .to
#'
#' @return
#' @export
#'
#' @examples
#'
convert_age <- function(.from, .to) {

  from_lt = as.POSIXlt(.from)
  to_lt = as.POSIXlt(.to)

  age <- to_lt$year - from_lt$year

  if_else(
    to_lt$mon < from_lt$mon |
      (to_lt$mon == from_lt$mon & to_lt$mday < from_lt$mday),
    age - 1L,
    age
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

mutate_line_number <- function(.data) {

  ln <- .data |> group_by(case_id) |> count()
  max <- max(ln$n, na.rm = T)

  if(max > 0) {

    for(i in seq_along(max)) {
      .data <- .data |>
        mutate(line_number = if_else(
          case_id == lag(case_id) & is.na(line_number),
          as.integer(lag(line_number)) + 1L,
          line_number
        )
        )
    }
  }

  return(.data)
}


