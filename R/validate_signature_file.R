#' Title
#'
#' @param .sig_folder
#' @param .area_code
#' @param .formats
#' @param .options
#'
#' @return
#' @export
#'
#' @examples
#'

read_signature_files <- function(
  .sig_folder,
  .area_code,
  .formats = c('png', 'jpg', 'jpeg'),
  .options = list(
    ink_threshold = 375,
    img_threshold = 100000,
    crop_threshold = 1000,
    filename_length = 31
  )
) {

  match_file_name <- paste0('^\\d{', nchar(.area_code), '}')
  file_formats <- paste0('\\.(', paste0(.formats, collapse = '|'), ')$')

  signature_folder <- file.info(
      list.files(
        file.path(.sig_folder, .area_code),
        full.names = T,
        pattern = file_formats,
        ignore.case = T,
        recursive = T
      )
    ) |>
    dplyr::as_tibble(rownames = 'file') |>
    dplyr::mutate(
      case_id = stringr::str_remove_all(basename(file), file_formats)
    ) |>
    dplyr::filter(!grepl('/backup/', file, ignore.case = T)) |>
    dplyr::select(file, case_id, size) |>
    filter_by_area() |>
    dplyr::mutate(
      is_invalid_filename = nchar(case_id) != .options$filename_length,
      is_above_threshold = size > .options$img_threshold,
      is_blank_canvas = purrr::map_int(file, get_image_value) == 0,
      is_insufficient_ink = .options$ink_threshold > purrr::map_int(file, get_image_value),
      is_corrupted = purrr::map_lgl(file, check_corrupted_image),
      is_cropped = purrr::map_int(file, check_cropped_image) > .options$crop_threshold
    )

  signature_folder

}

check_cropped_image <- function(file) {

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
    is_corrupted <- TRUE
  }

  img <- magick::image_read(file)
  if(length(img) == 0) {
    is_corrupted <- TRUE
  }

  return(is_corrupted)

}


get_image_value <- function(file) {

  tryCatch({
    img <- magick::image_read(file)
    l <- as.numeric(magick::image_resize(img, '300x300')[[1]][1, , ])
    length(l[l < 225])
  },

  error = function(e) {
    stop('Invalid image file')
  })
}

