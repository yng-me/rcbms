#' Title
#'
#' @param .dir
#' @param .area_code
#' @param .sig_folder
#' @param .formats
#' @param .options
#'
#' @return
#' @export
#'
#' @examples
#'

read_signature_files <- function(
  .dir,
  .area_code = NULL,
  .sig_folder = NULL,
  .formats = c('png', 'jpg', 'jpeg'),
  .options = list(
    ink_threshold = 375,
    img_threshold = 100000,
    crop_threshold = 1000,
    filename_length = 31
  )
) {

  match_file_name <- paste0('^\\d{', nchar(.area_code), '}')
  format <- paste0('\\.(', paste0(.formats, collapse = '|'), ')$')
  file_formats <- paste0(.area_code, '.*\\.(', paste0(.formats, collapse = '|'), ')$')

  path <- .dir

  if(!is.null(.area_code)) {

    path <- file.path(.dir, .area_code)

    if(!is.null(.sig_folder)) {
      path <- file.path(.dir, .area_code, .sig_folder)
    }

  }

  signature_folder <- file.info(
      list.files(
        path,
        full.names = T,
        pattern = file_formats,
        ignore.case = T,
        recursive = T
      )
    ) |>
    dplyr::as_tibble(rownames = 'file') |>
    dplyr::mutate(
      case_id = stringr::str_remove_all(basename(file), format)
    ) |>
    dplyr::select(file, case_id, size) |>
    filter_by_area() |>
    dplyr::mutate(
      is_invalid_filename = nchar(case_id) != .options$filename_length,
      is_above_threshold = size > .options$img_threshold,
      is_blank_canvas = purrr::map_int(file, get_image_value) == 0,
      is_insufficient_ink = .options$ink_threshold > purrr::map_int(file, get_image_value),
      is_corrupted = purrr::map_lgl(file, check_corrupted_image),
      is_cropped = purrr::map_int(file, check_cropped_image) > .options$crop_threshold & !is_above_threshold
    )

  signature_folder

}

#' Title
#'
#' @param file
#'
#' @return
#' @export
#'
#' @examples
check_cropped_image <- function(file) {

  if(check_corrupted_image(file)) return(0)

  img <- magick::image_read(file)
  l <- magick::image_resize(img, '300x300')

  ll <- l[[1]][1, , ] |>
    as.data.frame() |>
    mutate_all(as.numeric)

  left <- dplyr::select(ll, 1) |>
    dplyr::rename(v = 1) |>
    dplyr::filter(v < 225) |>
    dplyr::pull() |>
    sum()

  right <- dplyr::select(ll, ncol(ll)) |>
    dplyr::rename(v = 1) |>
    dplyr::filter(v < 225) |>
    dplyr::pull() |>
    sum()

  top <- head(ll, n = 1) |>
    tidyr::pivot_longer(dplyr::everything()) |>
    dplyr::filter(value < 255) |>
    dplyr::pull(value) |>
    sum()

  bottom <- tail(ll, n = 1) |>
    tidyr::pivot_longer(dplyr::everything()) |>
    dplyr::filter(value < 255) |>
    dplyr::pull(value) |>
    sum()

  pv <- left + right + top + bottom

  return(pv)

}

#' Title
#'
#' @param file
#'
#' @return
#' @export
#'
#' @examples
#'
check_corrupted_image <- function(file) {

  header <- readBin(file, what = "raw", n = 10)
  is_corrupted <- FALSE

  if(!identical(header[7:10], charToRaw("JFIF")) &
     !(identical(header[1], as.raw(137)) &
       identical(header[2], as.raw(80)) &
       identical(header[3], as.raw(78)) &
       identical(header[4], as.raw(71)) &
       identical(header[5], as.raw(13)) &
       identical(header[6], as.raw(10)) &
       identical(header[7], as.raw(26)) &
       identical(header[8], as.raw(10))
     )
  ) {
    return(TRUE)
  }

  img <- magick::image_read(file)
  if(length(img) == 0) {
    is_corrupted <- TRUE
  }

  return(is_corrupted)

}


#' Title
#'
#' @param file
#'
#' @return
#' @export
#'
#' @examples
get_image_value <- function(file) {

  if(check_corrupted_image(file)) return(99999)

  img <- magick::image_read(file)
  l <- as.numeric(magick::image_resize(img, '300x300')[[1]][1, , ])
  length(l[l < 225])
}

