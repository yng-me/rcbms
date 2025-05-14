# import_data_chunk <- function(
#   .file,
#   .chunk_path,
#   .chunk_size = 250000,
#   .chunk_by = "province_code",
#   .config = getOption("rcbms.config")
# ) {
#
#   readr::read_tsv_chunked(
#     file,
#     SideEffectChunkCallback$new(
#       function(x, pos) {
#
#         n <- pos + .chunk_size - 1
#         n_label <- stringr::str_pad(n, width = '9', pad = '0')
#
#         area <- x |>
#           dplyr::pull(!!as.name(.chunk_by)) |>
#           unique()
#
#         for(i in seq_along(area)) {
#
#
#
#           prov_label <- areas |>
#             filter(province_code == as.integer(area[y])) |>
#             pull(filename)
#
#           if(length(prov_label) > 0) {
#
#             prov_path <- join_path(path, prov_label[1])
#             create_new_folder(prov_path)
#
#             df <- x |>
#               filter(!!as.name(.area_filter) == area[y]) |>
#               clean_colnames() |>
#
#
#             write_parquet(
#               df,
#               paste0(prov_path, '/', file_name, '_', n_label, '.parquet')
#             )
#           }
#         }
#
#       }
#     ),
#     chunk_size = .chunk_size,
#     quote = "",
#     trim_ws = TRUE,
#     skip_empty_rows = TRUE
#   )
# }
