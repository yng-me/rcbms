# validate_signature_file <- function(
#   .data,
#   ...,
#   .config = getOption("rcbms.config")
# ) {
#
#   if(.config$mode$edit != 4) return(invisible(NULL))
#
#   ref_cv <- list()
#
#   ref_corruted_files <- NULL
#   ref_insufficient_ink <- NULL
#   ref_blank_canvas <- NULL
#   ref_valid_signature <- NULL
#   ref_unsupported <- NULL
#   ref_excluded_files <- NULL
#   signature_files <- NULL
#
#   sig <- .config$validation$signature
#   path <- sig$path
#
#   valid_folder <- create_new_folder(file.path(sig$valid$path, 'Valid'))
#   invalid_folder <- create_new_folder(file.path(sig$invalid$path, 'Invalid'))
#
#   corrputed <- create_new_folder(file.path(invalid_folder, 'Corrupted files'))
#   insufficient_ink <- create_new_folder(file.path(invalid_folder, 'Insufficient ink'))
#   blank <- create_new_folder(file.path(invalid_folder, 'Blank canvas'))
#   excluded_sig <- create_new_folder(file.path(invalid_folder, 'Excluded files'))
#
#   signature_folder <- list.files(path, full.names = T, ignore.case = T) |>
#     as_tibble() |>
#     mutate(folder = basename(value)) |>
#     filter(
#       !grepl('BACKUP', value, ignore.case = T),
#       grepl(paste0("^\\d{", 15 + config$project$add_length, "}$"), folder)
#     ) |>
#     pull(value)
#
#   for(i in seq_along(signature_folder)) {
#
#     sf_temp <- file.info(
#       list.files(
#         signature_folder[i],
#         full.names = T,
#         pattern = '\\.(png|jpg|jpeg)$',
#         ignore.case = T,
#         recursive = T
#       )) |>
#       as_tibble(rownames = 'file') |>
#       mutate(case_id = str_remove_all(basename(file), '\\.(png|jpg|jpeg)$')) |>
#       select(file, case_id, size) |>
#       mutate(barangay_geo = str_sub(case_id, 1, 9 + config$project$add_length)) |>
#       filter(!grepl('/backup/', file, ignore.case = T))
#
#     signature_files <- signature_files |>
#       bind_rows(sf_temp)
#   }
#
#   if(!is.null(signature_files)) {
#     agreed_sign_waiver_caseids <- .data |>
#       filter(agree_to_sign_the_waiver == 1) |>
#       select_cv(agree_to_sign_the_waiver)
#
#     signature_files_excluded <- signature_files |>
#       filter(!(case_id %in% agreed_sign_waiver_caseids$case_id)) |>
#       pull(file)
#
#     if(length(signature_files_excluded) > 0) {
#       for(j in seq_along(signature_files_excluded)) {
#         file.copy(signature_files_excluded[j],  excluded_sig)
#         file.remove(signature_files_excluded[j])
#         ref_excluded_files <- c(ref_excluded_files, signature_files_excluded[j])
#       }
#     }
#
#     signature_files <- signature_files |>
#       filter(!(file %in% ref_excluded_files))
#   }
#
#   if(!is.null(signature_files)) {
#
#     # No matching signatures -----------------------------------------------------
#     cv$cv_1_no_signature <- .data |>
#       filter(
#         !(case_id %in% signature_files$case_id),
#         agree_to_sign_the_waiver == 1
#       ) |>
#       select_cv(agree_to_sign_the_waiver)
#
#     # Corrupted file ---------------------------------------------------------------
#
#     for(i in seq_along(signature_files$file)) {
#
#       x <- signature_files$file[i]
#       header <- readBin(x, what = "raw", n = 10)
#
#       if(!identical(header[7:10], charToRaw("JFIF")) &
#          !(identical(header[1], as.raw(137)) &
#            identical(header[2], as.raw(80)) &
#            identical(header[3], as.raw(78)) &
#            identical(header[4], as.raw(71)) &
#            identical(header[5], as.raw(13)) &
#            identical(header[6], as.raw(10)) &
#            identical(header[7], as.raw(26)) &
#            identical(header[8], as.raw(10))
#          )
#       ) {
#
#         file.copy(x, corrputed)
#         file.remove(x)
#         ref_corruted_files <- c(ref_corruted_files, x)
#
#       }
#     }
#
#     signature_files <- signature_files |>
#       filter(!(file %in% ref_corruted_files))
#
#     # Signed Form 3 manually using PAPI --------------------------------------------
#     signed_manually <- signature_files |>
#       filter(size >= img_threshold, nchar(case_id) == 27) |>
#       pull(file)
#
#     for(j in seq_along(signed_manually)) {
#       y <- signed_manually[j]
#       ref_valid_signature <- c(ref_valid_signature, y)
#       file.copy(y, valid_folder)
#       file.remove(y)
#     }
#
#     # For inspection ---------------------------------------------------------------
#     signed_either_manually_or_directly <- signature_files |>
#       filter(size < img_threshold, nchar(case_id) == 27) |>
#       pull(file)
#
#     # Invalidation file names ------------------------------------------------------
#     invalid_filenames <- signature_files |>
#       filter(nchar(case_id) != 27) |>
#       pull(file)
#
#     invalid_filename <- create_new_folder(join_path(invalid_folder, 'Invalid file name'))
#
#     if(length(invalid_filenames) > 0) {
#       for(j in seq_along(invalid_filenames)) {
#         file.copy(invalid_filenames[j],  invalid_filename)
#         file.remove(invalid_filenames[j])
#       }
#     }
#
#
#     for(j in seq_along(signed_either_manually_or_directly)) {
#
#       tryCatch({
#         y <- signed_either_manually_or_directly[j]
#         img <- magick::image_read(y)
#
#         l <- as.numeric(magick::image_resize(img, '300x300')[[1]][1, , ])
#         p <- length(l[l < 225])
#
#         if(length(img) == 0) {
#           ref_corruted_files <- c(ref_corruted_files, y)
#           file.copy(y,  corrputed)
#           file.remove(y)
#
#         } else if(p == 0) {
#           ref_blank_canvas <- c(ref_blank_canvas, y)
#           file.copy(y,  blank)
#           file.remove(y)
#
#         } else if(p < ink_threshold) {
#           ref_insufficient_ink <- c(ref_insufficient_ink, y)
#           file.copy(y, insufficient_ink)
#           file.remove(y)
#
#         } else {
#           ref_valid_signature <- c(ref_valid_signature, y)
#           file.copy(y, valid_folder)
#           file.remove(y)
#
#         }
#       }, error = function(e) {
#         cat("Error in processing ", y , "\n")
#         ref_corruted_files <<- c(ref_corruted_files, y)
#         file.copy(y,  corrputed)
#         file.remove(y)
#       })
#     }
#
#     if(length(ref_corruted_files) > 0) {
#       cv$cv_1_corrupted_file <- ref_corruted_files |>
#         as_tibble() |>
#         mutate(case_id = str_remove_all(basename(value), '\\.(png|jpg|jpeg)$')) |>
#         left_join(.data, by = 'case_id') |>
#         select_cv()
#     }
#
#     if(length(ref_insufficient_ink) > 0) {
#       cv$cv_1_insufficient_ink <- ref_insufficient_ink |>
#         as_tibble() |>
#         mutate(case_id = str_remove_all(basename(value), '\\.(png|jpg|jpeg)$')) |>
#         left_join(.data, by = 'case_id') |>
#         select_cv()
#     }
#
#
#     if(length(ref_blank_canvas) > 0) {
#       cv$cv_1_blank_canvas <- ref_blank_canvas |>
#         as_tibble() |>
#         mutate(case_id = str_remove_all(basename(value), '\\.(png|jpg|jpeg)$')) |>
#         left_join(.data, by = 'case_id') |>
#         select_cv()
#     }
#   }
#
#   #removing Empty Folders
#   empty_subfolder <- list.files(
#     paste0(signature_path,'/',agg_area_code),
#     full.names = T,
#     ignore.case = T
#   ) |>
#     as_tibble() |>
#     mutate(folder = basename(value)) |>
#     filter(
#       !grepl('BACKUP', value, ignore.case = T),
#       grepl('^\\d{15}$', folder),
#       grepl(paste0("^", agg_area_code), folder)
#     ) |>
#     pull(value)
#
#   empty_folder <- list.files(signature_path, full.names = T, ignore.case = T) |>
#     as_tibble() |>
#     mutate(folder = basename(value)) |>
#     filter(!grepl('BACKUP', value, ignore.case = T), grepl('^\\d{6}$', folder), grepl(paste0("^", agg_area_code), folder)) |>
#     pull(value)
#
#
#   if(length(empty_subfolder) > 0) {
#     for (j in seq_along(empty_subfolder)) {
#       if (length(dir(empty_subfolder[j])) == 0) {
#         unlink(empty_subfolder[j], recursive = TRUE)
#       }
#     }
#   }
#
#
#   if(length(empty_folder) > 0) {
#     for (j in seq_along(empty_folder)) {
#       if (length(dir(empty_folder[j])) == 0) {
#         unlink(empty_folder[j], recursive = TRUE)
#       }
#     }
#   }
#
# }
