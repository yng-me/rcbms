# validate_signature_file <- function(
#   path,
#   hh_summary,
#   ink_threshold = 375,
#   img_threshold = 100000
# ) {
#
#   ref_cv <- list()
#
#   ref_corruted_files <- NULL
#   ref_insufficient_ink <- NULL
#   ref_blank_canvas <- NULL
#   ref_valid_signature <- NULL
#   ref_unsupported <- NULL
#   ref_excluded_files <- NULL
#
#
#
#   # Create folder for storing invalid signatures
#   valid_folder <- create_new_folder(
#     join_path(config$project$validation$paths$archive_path, paste0(agg_area_code,' - ',agg_area_name), '/Valid')
#   )
#
#   invalid_folder <- create_new_folder(
#     join_path(config$project$validation$paths$archive_path, paste0(agg_area_code,' - ',agg_area_name), '/Invalid')
#   )
#
#   corrputed <- create_new_folder(join_path(invalid_folder, 'Corrupted files'))
#   insufficient_ink <- create_new_folder(join_path(invalid_folder, 'Insufficient ink'))
#   blank <- create_new_folder(join_path(invalid_folder, 'Blank canvas'))
#   excluded_sig <- create_new_folder(join_path(invalid_folder, 'Excluded files'))
#
#
#   signature_folder <- list.files(
#     paste0(signature_path,'/',agg_area_code), #changed the source to aggregate area code
#     full.names = T,
#     ignore.case = T
#   ) %>%
#     as_tibble() %>%
#     mutate(folder = basename(value)) %>%
#     filter(!grepl('BACKUP', value, ignore.case = T), grepl('^\\d{15}$', folder)) %>%
#     pull(value)
#
#   signature_files <- NULL
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
#       )) %>% as_tibble(rownames = 'file') %>%
#       mutate(case_id = str_remove_all(basename(file), '\\.(png|jpg|jpeg)$')) %>%
#       select(file, case_id, size) %>%
#       mutate(barangay_geo = str_sub(case_id, 1, 9)) %>%
#       filter(!grepl('/backup/', file, ignore.case = T)) %>%
#       join_and_filter_area()
#
#     signature_files <- signature_files %>%
#       bind_rows(sf_temp)
#   }
#
#   progress_counter <- nrow(signature_files)
#
#   print_progress(cat(cyan(bold(paste0('Validating signature files... \n')))))
#
#   if(!is.null(signature_files)) {
#     agreed_sign_waiver_caseids <- hh_summary %>%
#       filter(agree_to_sign_the_waiver == 1) %>%
#       select_cv(agree_to_sign_the_waiver)
#
#     progress_counter <- nrow(agreed_sign_waiver_caseids)
#
#     signature_files_excluded <- signature_files %>%
#       filter(
#         !(case_id %in% agreed_sign_waiver_caseids$case_id)
#       ) %>% pull(file)
#
#     if(length(signature_files_excluded) > 0) {
#       for(j in seq_along(signature_files_excluded)) {
#         excluded_sig_filename <- paste0(excluded_sig,"\\", basename(signature_files_excluded[j]))
#         file.rename(signature_files_excluded[j],  excluded_sig_filename)
#         ref_excluded_files <- c(ref_excluded_files, signature_files_excluded[j])
#       }
#     }
#
#     signature_files <- signature_files %>%
#       filter(!(file %in% ref_excluded_files))
#   }
#
#   if(!is.null(signature_files)) {
#
#     if(!exists('cv')) cv <- list()
#
#
#     # No matching signatures -----------------------------------------------------
#     cv$cv_1_no_signature <- hh_summary %>%
#       filter(
#         !(case_id %in% signature_files$case_id),
#         agree_to_sign_the_waiver == 1
#       ) %>%
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
#         ref_corruted_files <- c(ref_corruted_files, x)
#         file.copy(x,  corrputed)
#         #file.remove(x)
#
#
#       }
#     }
#
#     # print_progress(cat(cyan(paste0('Progress: ',
#     #                                round((length(ref_excluded_files)+length(ref_corruted_files))
#     #                                      /progress_counter,2),
#     #                                '% Completed \n'))))
#
#     signature_files <- signature_files %>%
#       filter(!(file %in% ref_corruted_files))
#
#     # Signed Form 3 manually using PAPI --------------------------------------------
#     signed_manually <- signature_files %>%
#       filter(size >= img_threshold, nchar(case_id) == 27) %>%
#       pull(file)
#
#     for(j in seq_along(signed_manually)) {
#       y <- signed_manually[j]
#       ref_valid_signature <- c(ref_valid_signature, y)
#       signed_manually_sig_filename <- paste0(valid_folder,"\\", basename(ref_valid_signature[j]))
#       #file.copy(y, signed_manually_sig_filename)
#
#     }
#     # print_progress(cat(cyan(paste0('Progress: ',
#     #                                round((length(ref_excluded_files)+length(ref_corruted_files)+length(ref_valid_signature))
#     #                                      /progress_counter,2),
#     #                                '% Completed \n'))))
#
#
#     # For inspection ---------------------------------------------------------------
#     signed_either_manually_or_directly <- signature_files %>%
#       filter(size < img_threshold, nchar(case_id) == 27) %>%
#       pull(file)
#
#     # Invalidation file names ------------------------------------------------------
#     invalid_filenames <- signature_files %>%
#       filter(nchar(case_id) != 27) %>%
#       pull(file)
#
#     invalid_filename <- create_new_folder(join_path(invalid_folder, 'Invalid file name'))
#
#     if(length(invalid_filenames) > 0) {
#       for(j in seq_along(invalid_filenames)) {
#
#         invalid_sig_filename <- paste0(invalid_filename,"\\", basename(invalid_filenames[j]))
#         file.copy(invalid_filenames[j],  invalid_sig_filename)
#
#       }
#     }
#
#
#
#
#     for(j in seq_along(signed_either_manually_or_directly)) {
#
#       # if(j %% 10 == 0) {
#       #
#       #     cat('\r',cyan('Progress: '),cyan(round((length(ref_excluded_files)+length(ref_corruted_files)+length(ref_valid_signature)+j)
#       #                    /nrow(progress_counter),2)),cyan('% Completed \n'))
#       #     flush.console()
#       #
#       #   }
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
#           #file.remove(y)
#
#         } else if(p == 0) {
#           ref_blank_canvas <- c(ref_blank_canvas, y)
#           file.copy(y,  blank)
#           #file.remove(y)
#
#         } else if(p < ink_threshold) {
#           ref_insufficient_ink <- c(ref_insufficient_ink, y)
#           file.copy(y, insufficient_ink)
#           #file.remove(y)
#
#         } else {
#           ref_valid_signature <- c(ref_valid_signature, y)
#           #file.copy(y, valid_folder)
#           #file.remove(y)
#
#         }
#       }, error = function(e) {
#         cat("Error in processing ", y , "\n")
#         ref_corruted_files <<- c(ref_corruted_files, y)
#         file.copy(y,  corrputed)
#         #file.remove(y)
#       })
#     }
#
#     if(length(ref_corruted_files) > 0) {
#       cv$cv_1_corrupted_file <- ref_corruted_files %>%
#         as_tibble() %>%
#         mutate(case_id = str_remove_all(basename(value), '\\.(png|jpg|jpeg)$')) %>%
#         left_join(hh_summary, by = 'case_id') %>%
#         select_cv()
#     }
#
#     if(length(ref_insufficient_ink) > 0) {
#       cv$cv_1_insufficient_ink <- ref_insufficient_ink %>%
#         as_tibble() %>%
#         mutate(case_id = str_remove_all(basename(value), '\\.(png|jpg|jpeg)$')) %>%
#         left_join(hh_summary, by = 'case_id') %>%
#         select_cv()
#     }
#
#
#     if(length(ref_blank_canvas) > 0) {
#       cv$cv_1_blank_canvas <- ref_blank_canvas %>%
#         as_tibble() %>%
#         mutate(case_id = str_remove_all(basename(value), '\\.(png|jpg|jpeg)$')) %>%
#         left_join(hh_summary, by = 'case_id') %>%
#         select_cv()
#     }
#   }
#
#
#
#   empty_folder <- list.files(signature_path, #changed the source to aggregate area code
#                              full.names = T,
#                              ignore.case = T
#   ) %>%
#     as_tibble() %>%
#     mutate(folder = basename(value)) %>%
#     filter(!grepl('BACKUP', value, ignore.case = T), grepl('^\\d{6}$', folder), grepl(paste0("^", agg_area_code), folder)) %>%
#     pull(value)
#
#
#   if(length(empty_folder) > 0) {
#     for (j in seq_along(empty_folder)) {
#       if (length(dir(empty_folder[j])) == 0) {
#         unlink(empty_folder[j], recursive = TRUE, force = TRUE)
#       }
#     }
#   }
#
#   print_progress(cat(magenta(paste0('Number of Excluded Files: ', length(ref_excluded_files), '\n',
#                                     'Number of Corrupted Files: ', length(ref_corruted_files), '\n',
#                                     'Number of Insufficient Ink: ', length(ref_insufficient_ink), '\n',
#                                     'Number of Blank Canvas: ', length(ref_blank_canvas), '\n',
#                                     'Number of No Signature: ', nrow(cv$cv_1_no_signature), '\n',
#                                     yellow('Total Number of Errors: ', sum(length(ref_corruted_files),
#                                                                            length(ref_insufficient_ink),
#                                                                            length(ref_blank_canvas),
#                                                                            nrow(cv$cv_1_no_signature)), '\n',
#                                            'Number of Valid Signature: ', length(ref_valid_signature), '\n')
#   ))))
#
#   if (length(ref_corruted_files) == 0 &
#       length(ref_insufficient_ink) == 0 &
#       length(ref_blank_canvas) == 0 &
#       nrow(cv$cv_1_no_signature) == 0) {
#
#     print_progress(cat(cyan(bold(paste0('Checking completeness of signature files... \n')))))
#
#     hh_summary <- parquet$hp$summary %>%
#       join_and_filter_area() %>%
#       collect()
#
#     config$project$validation$priority_levels <- unique(
#       c('A', config$project$validation$priority_levels)
#     )
#
#     # Create folder for storing invalid signatures
#     valid_folder <- create_new_folder(
#       join_path(config$project$validation$paths$archive_path, paste0(agg_area_code,' - ',agg_area_name), '/Valid')
#     )
#
#     invalid_folder <- create_new_folder(
#       join_path(config$project$validation$paths$archive_path, paste0(agg_area_code,' - ',agg_area_name), '/Invalid')
#     )
#
#     signature_folder <- list.files(
#       paste0(signature_path,'/',agg_area_code), #changed the source to aggregate area code
#       full.names = T,
#       ignore.case = T
#     ) %>%
#       as_tibble() %>%
#       mutate(folder = basename(value)) %>%
#       filter(!grepl('BACKUP', value, ignore.case = T), grepl('^\\d{15}$', folder)) %>%
#       pull(value)
#
#     signature_files <- NULL
#
#     for(i in seq_along(signature_folder)) {
#
#       sf_temp <- file.info(
#         list.files(
#           signature_folder[i],
#           full.names = T,
#           pattern = '\\.(png|jpg|jpeg)$',
#           ignore.case = T,
#           recursive = T
#         )) %>% as_tibble(rownames = 'file') %>%
#         mutate(case_id = str_remove_all(basename(file), '\\.(png|jpg|jpeg)$')) %>%
#         select(file, case_id, size) %>%
#         mutate(barangay_geo = str_sub(case_id, 1, 9)) %>%
#         filter(!grepl('/backup/', file, ignore.case = T)) %>%
#         join_and_filter_area()
#
#       signature_files <- signature_files %>%
#         bind_rows(sf_temp)
#     }
#
#
#     #Checking if there are cases without signature
#     no_signature <- hh_summary %>%
#       filter(
#         !(case_id %in% signature_files$case_id),
#         agree_to_sign_the_waiver == 1
#       ) %>%
#       select_cv(agree_to_sign_the_waiver)
#
#     print_progress(cat(cyan(bold(paste0('Done... \n')))))
#
#     if (nrow(no_signature) == 0) {
#
#       print_progress(cat(cyan(bold(paste0('Sorting signature files... \n')))))
#
#       #list of HH which agreed to sign waiver
#       agreed_sign_waiver_caseids <- hh_summary %>%
#         filter(agree_to_sign_the_waiver == 1) %>%
#         select_cv(agree_to_sign_the_waiver)
#
#       #list of signatures that is excluded
#       signature_files_excluded <- signature_files %>%
#         filter(
#           !(case_id %in% agreed_sign_waiver_caseids$case_id)
#         ) %>% pull(file)
#
#       if(length(signature_files_excluded) > 0) {
#         for(j in seq_along(signature_files_excluded)) {
#           excluded_sig_filename <- paste0(invalid_folder,"\\", basename(signature_files_excluded[j]))
#           file.rename(signature_files_excluded[j],  excluded_sig_filename)
#           ref_excluded_files <- c(ref_excluded_files, signature_files_excluded[j])
#         }
#       }
#
#       signature_files <- signature_files %>%
#         filter(!(file %in% ref_excluded_files)) |>
#         pull(file)
#
#       for(j in seq_along(signature_files)) {
#         valid_sig_filename <- paste0(valid_folder,"\\", basename(signature_files[j]))
#         file.rename(signature_files[j],  valid_sig_filename)
#       }
#
#       #a <- list.files(valid_folder)
#       print_progress(cat(cyan(bold(paste0('Done... ')))))
#
#     } else{
#       print_progress(cat(cyan(bold(paste0('Cases without signature detected... ')))))
#     }
#
#   }
#
#
#
#
#   } else {
#   print_progress(cat(cyan(bold(paste0('Checking completeness of signature files... \n')))))
#
#   hh_summary <- parquet$hp$summary %>%
#     join_and_filter_area() %>%
#     collect()
#
#   config$project$validation$priority_levels <- unique(
#     c('A', config$project$validation$priority_levels)
#   )
#
#   # Create folder for storing invalid signatures
#   valid_folder <- create_new_folder(
#     join_path(config$project$validation$paths$archive_path, paste0(agg_area_code,' - ',agg_area_name), '/Valid')
#   )
#
#   invalid_folder <- create_new_folder(
#     join_path(config$project$validation$paths$archive_path, paste0(agg_area_code,' - ',agg_area_name), '/Invalid')
#   )
#
#   signature_folder <- list.files(
#     paste0(signature_path,'/',agg_area_code), #changed the source to aggregate area code
#     full.names = T,
#     ignore.case = T
#   ) %>%
#     as_tibble() %>%
#     mutate(folder = basename(value)) %>%
#     filter(!grepl('BACKUP', value, ignore.case = T), grepl('^\\d{15}$', folder)) %>%
#     pull(value)
#
#   signature_files <- NULL
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
#       )) %>% as_tibble(rownames = 'file') %>%
#       mutate(case_id = str_remove_all(basename(file), '\\.(png|jpg|jpeg)$')) %>%
#       select(file, case_id, size) %>%
#       mutate(barangay_geo = str_sub(case_id, 1, 9)) %>%
#       filter(!grepl('/backup/', file, ignore.case = T)) %>%
#       join_and_filter_area()
#
#     signature_files <- signature_files %>%
#       bind_rows(sf_temp)
#   }
#
#
#   #Checking if there are cases without signature
#   no_signature <- hh_summary %>%
#     filter(
#       !(case_id %in% signature_files$case_id),
#       agree_to_sign_the_waiver == 1
#     ) %>%
#     select_cv(agree_to_sign_the_waiver)
#
#   print_progress(cat(cyan(bold(paste0('Done... ')))))
#
#   if (nrow(no_signature) == 0) {
#
#     print_progress(cat(cyan(bold(paste0('Sorting signature files... ')))))
#
#     #list of HH which agreed to sign waiver
#     agreed_sign_waiver_caseids <- hh_summary %>%
#       filter(agree_to_sign_the_waiver == 1) %>%
#       select_cv(agree_to_sign_the_waiver)
#
#     #list of signatures that is excluded
#     signature_files_excluded <- signature_files %>%
#       filter(
#         !(case_id %in% agreed_sign_waiver_caseids$case_id)
#       ) %>% pull(file)
#
#     if(length(signature_files_excluded) > 0) {
#       for(j in seq_along(signature_files_excluded)) {
#         excluded_sig_filename <- paste0(invalid_folder,"\\", basename(signature_files_excluded[j]))
#         file.rename(signature_files_excluded[j],  excluded_sig_filename)
#         ref_excluded_files <- c(ref_excluded_files, signature_files_excluded[j])
#       }
#     }
#
#     signature_files <- signature_files %>%
#       filter(!(file %in% ref_excluded_files))
#
#     for(j in seq_along(signature_files)) {
#       valid_sig_filename <- paste0(valid_folder,"\\", basename(signature_files[j]))
#       file.rename(signature_files[j],  valid_sig_filename)
#     }
#
#     #a <- list.files(valid_folder)
#     print_progress(cat(cyan(bold(paste0('Done... ')))))
#
#   } else{
#     print_progress(cat(cyan(bold(paste0('Cases without signature detected... ')))))
#   }
#
# }
