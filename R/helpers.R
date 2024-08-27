#' Title
#'
#' @param ...
#' @param .code_ref
#' @param .is_char
#' @param .ordered
#'
#' @return
#' @export
#'
#' @examples
convert_to_factor <- function(
    .x,
    ...,
    .code_ref = NULL,
    .is_char = FALSE,
    .ordered = FALSE) {
  refs <- get_config("references")
  if (is.null(refs)) {
    return(.x)
  }

  vs <- refs$valueset |>
    dplyr::filter(name == .code_ref) |>
    dplyr::distinct(label, .keep_all = T)

  if (.is_char == T) {
    vs <- vs |> dplyr::mutate(value = stringr::str_trim(value))
  } else {
    vs <- vs |> dplyr::mutate(value = as.integer(value))
  }

  if (!is.null(.code_ref)) {
    factor(
      .x,
      ...,
      levels = vs$value,
      labels = vs$label,
      ordered = .ordered
    )
  } else {
    factor(.x, ..., ordered = .ordered)
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
convert_to_na <- function(.data, .convert_value = "", .pattern = NULL) {
  if (!is.null(.pattern)) {
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
convert_age <- function(.from, .to) {
  from_lt <- as.POSIXlt(.from)
  to_lt <- as.POSIXlt(.to)

  age <- to_lt$year - from_lt$year

  dplyr::if_else(
    to_lt$mon < from_lt$mon |
      (to_lt$mon == from_lt$mon & to_lt$mday < from_lt$mday),
    age - 1L,
    age
  )
}




#' Title
#'
#' @param .data
#' @param .name
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
sum_rows <- function(.data, .name, ...) {
  df <- dplyr::select(.data, ...)
  s__ <- rowSums(df, na.rm = T)
  .data |>
    tibble::add_column(`s__`) |>
    dplyr::rename(!!as.name(.name) := `s__`)
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

sort_variable_names <- function(.data, .input_data, .config = getOption('rcbms.config')) {

  final_status <- .config$project[[.input_data]]$final_status$variable

  .data |>
    dplyr::select(
      dplyr::any_of(unique(c(
        'row_id',
        'case_id',
        'cbms_geoid',
        'barangay_geo',
        'geocode',
        'region_code',
        'province_code',
        'city_mun_code',
        'barangay_code',
        'ean',
        'bsn',
        'husn',
        'hsn',
        'isn',
        'line_number',
        'region',
        'province',
        'city_mun',
        'barangay',
        'total_population',
        'total_hh',
        'average_hh_size',
        'mode_of_data_collection',
        'date_of_visit',
        'time_began',
        'time_ended',
        final_status,
        "sex",
        "age"
      ))),
      sort(names(.data))
    )
}





#' Title
#'
#' @param .str
#' @param .config
#' @param .key
#' @param .iv
#'
#' @return
#' @export
#'
#' @examples
#'
encrypt_info <- function(.str, .config = getOption('rcbms.config'), .key = NULL, .iv = NULL) {

  use_encryption <- .config$use_encryption
  if(is.null(use_encryption)) {
    use_encryption <- FALSE
  }

  if(use_encryption | !is.null(.key)) {

    if(is.null(.key)) {
      .key <- .config$env$AES_KEY
      iv <- hex_to_raw(.config$env$AES_IV)
    }

    if(!is.null(.iv)) {
      iv <- hex_to_raw(.iv)
    } else {
      iv <- openssl::rand_bytes(16)
    }

    .str <- .str |>
      serialize(connection = NULL) |>
      openssl::aes_cbc_encrypt(
        key = openssl::sha256(charToRaw(.key)),
        iv = iv
      ) |>
      openssl::base64_encode()

  } else {
    .str <- .str |>
      serialize(connection = NULL) |>
      openssl::base64_encode()
  }

  return(.str)
}


#' Title
#'
#' @param .str
#' @param .config
#' @param .key
#' @param .iv
#'
#' @return
#' @export
#'
#' @examples

decrypt_info <- function(.str, .config = getOption('rcbms.config'), .key = NULL, .iv = NULL) {

  use_encryption <- .config$use_encryption
  if(is.null(use_encryption)) {
    use_encryption <- FALSE
  }

  if(use_encryption | !is.null(.key)) {

    if(is.null(.key)) {
      .key <- .config$env$AES_KEY
      iv <- hex_to_raw(.config$env$AES_IV)
    }

    if(!is.null(.iv)) {
      iv <- hex_to_raw(.iv)
    } else {
      iv <- openssl::rand_bytes(16)
    }

    .str <- .str |>
      openssl::base64_decode() |>
      openssl::aes_cbc_decrypt(
        key = openssl::sha256(charToRaw(.key)),
        iv = iv
      ) |>
      unserialize()

  } else {
    .str <- .str |>
      openssl::base64_decode() |>
      unserialize()
  }

  return(.str)
}
