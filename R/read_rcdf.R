#' Title
#'
#' @param path
#' @param encryption_key
#' @param metadata
#'
#' @return
#' @export
#'
#' @examples

read_rcdf <- function(path, encryption_key, metadata = NULL) {

  meta <- import_rcdf(path)
  aes_key <- decrypt_info(meta$key_app, .key = encryption_key)
  aes_iv <- decrypt_info(meta$iv_app, .key = encryption_key)

  pq_path <- file.path(meta$dir, 'lineage')
  pq_files <- list.files(pq_path, pattern = '\\.parquet', full.names = T)
  pq_files <- pq_files[!grepl('adtl_qtn', pq_files)]

  pq <- list()

  for(i in seq_along(pq_files)) {
    pq_file <- pq_files[i]
    record <- fs::path_ext_remove(basename(pq_file))

    pq_temp <- read_encryped_parquet(pq_file, aes_key, aes_iv, as_arrow_table = F)

    if(!is.null(metadata)) {

      if(!is.null(metadata$data_dictionary) & !is.null(metadata$valueset)) {

        pq_temp <-add_metadata(
          .data = pq_temp,
          .dictionary = metadata$data_dictionary[[meta$input_data]],
          .valueset = metadata$valueset
        )
      }
    }

    pq[[record]] <- arrow::arrow_table(pq_temp)

  }

  attr(pq, 'metadata') <- meta

  pq

}

#' Title
#'
#' @param path
#'
#' @return
#' @export
#'
#' @examples
#'
import_rcdf <- function(path, meta_only = FALSE) {

  temp_dir <-  tempdir()
  temp_dir_to <- file.path(temp_dir, 'rcdf_temp', fs::path_ext_remove(basename(path)))

  unzip(path, exdir = temp_dir_to, junkpaths = T)
  if(!meta_only) {
    unzip(file.path(temp_dir_to, 'lineage.zip'), exdir = temp_dir_to)
  }
  unlink(file.path(temp_dir_to, 'lineage.zip'), recursive = T, force = T)

  meta <- jsonlite::read_json(file.path(temp_dir_to, 'metadata.json'), simplifyVector = T)

  meta$dir <- temp_dir_to
  meta
}


#' Title
#'
#' @param path
#' @param aes_key
#' @param aes_iv
#'
#' @return
#' @export
#'
#' @examples
#'
read_encryped_parquet <- function(path, aes_key = NULL, aes_iv = NULL, as_arrow_table = TRUE) {

  if(is.null(aes_key) | is.null(aes_iv)) {

    df <- arrow::open_dataset(path)

  } else {

    duckdb_conn <- DBI::dbConnect(duckdb::duckdb())
    q_encrypt <- paste0("PRAGMA add_parquet_key('", aes_key, "', '", aes_iv, "');")
    DBI::dbExecute(duckdb_conn, q_encrypt)

    q_pq <- paste0(
      "SELECT * FROM read_parquet('",
      path,
      "', encryption_config = { footer_key: '",
      aes_key,
      "' });"
    )

    df <- DBI::dbGetQuery(duckdb_conn, q_pq)

    if(as_arrow_table) {
      df <- arrow::arrow_table(df)
    }

    DBI::dbDisconnect(duckdb_conn, shutdown = TRUE)
  }


  return(df)

}


#' Title
#'
#' @param path
#' @param input_data
#' @param aes_key
#' @param aes_iv
#'
#' @return
#' @export
#'
#' @examples
#'

read_encryped_parquet_list <- function(path, input_data = NULL, aes_key = NULL, aes_iv = NULL) {

  pq <- list()
  pq_files <- list.files(path, pattern = '\\.parquet', full.names = T)
  pq_files <- pq_files[!grepl('adtl_qtn', pq_files)]

  for(i in seq_along(pq_files)) {

    pq_file <- pq_files[i]
    record <- fs::path_ext_remove(basename(pq_file))

    df <- rcbms::read_encryped_parquet(
      path = pq_file,
      aes_key = aes_key,
      aes_iv = aes_iv
    )

    if(!is.null(input_data)) {
      pq[[input_data]][[record]] <- df
    } else {
      pq[[record]] <- df
    }
  }

  return(pq)

}



#' Title
#'
#' @param data
#' @param path
#' @param aes_key
#' @param aes_iv
#'
#' @return
#' @export
#'
#' @examples
#'

write_encryped_parquet <- function(data, path, aes_key = NULL, aes_iv = NULL) {

  duckdb_conn <- DBI::dbConnect(duckdb::duckdb())
  q_encrypt <- paste0("PRAGMA add_parquet_key('", aes_key, "', '", aes_iv, "');")
  DBI::dbExecute(duckdb_conn, q_encrypt)

  q_to_pq <- paste0(
    "COPY df_temp TO '",
    path,
    "' (ENCRYPTION_CONFIG { footer_key: '", aes_key, "' });"
  )

  DBI::dbWriteTable(duckdb_conn, name = "df_temp", value = dplyr::collect(data), overwrite = T)
  DBI::dbExecute(duckdb_conn, q_to_pq)
  DBI::dbDisconnect(duckdb_conn, shutdown = TRUE)

}


#' Title
#'
#' @param text
#' @param n
#'
#' @return
#' @export
#'
#' @examples
#'

str_split_n <- function(text, n = 3) {

  text <- as.character(text)
  text_length <- nchar(text)

  text_rev <- strsplit(text, '')[[1]]
  text_rev <- paste0(rev(text_rev), collapse = '')

  text_seq <- unique(c(seq(from = 0, to = text_length, by = n), text_length))
  text_seq_trimmed <- text_seq[1:(length(text_seq) - 1)]

  value <- NULL

  for(i in seq_along(text_seq_trimmed)) {

    sub_from <- text_seq_trimmed[i] + 1
    sub_to <- text_seq[i + 1]

    value_i <- stringr::str_sub(text_rev, sub_from, sub_to)
    value_i <- stringr::str_pad(value_i, width = n, side = 'right', pad = '0')
    value_i <- strsplit(value_i, '')[[1]]
    value_i <- paste0(rev(value_i), collapse = '')

    value <- c(value_i, value)

  }

  value
}




#' Title
#'
#' @param paths
#' @param level
#' @param add_length
#'
#' @return
#' @export
#'
#' @examples
#'
get_complete_rcdf <- function(paths, level = 3, add_length = 1) {

  agg_level_length <- c(2, 4 + add_length, 6 + add_length)[level]

  ref_counter <- list(
    validation = list(
      hp = 14,
      bp = 1,
      ilq = 1,
      shp = 1
    ),
    transformation = list(
      hp = 6,
      bp = 1,
      ilq = 1,
      shp = 1
    )
  )

  x <- list()

  for(i in seq_along(paths)) {

    meta <- rcbms::import_rcdf(paths[i], meta_only = T)

    if(file.exists(meta$dir)) {
      unlink(meta$dir, recursive = T, force = T)
    }

    if(grepl('[^0-9]', meta$area_code)) next

    mode <- meta$mode
    input_data <- meta$input_data
    meta_counter <- nrow(meta$checksum) >= ref_counter[[mode]][[input_data]]
    if(!meta_counter) next

    if(nchar(meta$area_code) < add_length + 6) {




    } else {

      area_code <- meta$area_code |>
        stringr::str_sub(1, agg_level_length) |>
        stringr::str_pad(width = 9 + add_length, side = 'right', pad = '0')

      x[[i]] <- list(
        area_code = area_code,
        region_code = stringr::str_sub(area_code, 1, 2),
        province_code = stringr::str_sub(area_code, 3, 4 + add_length),
        city_mun_code = stringr::str_sub(area_code, 5 + add_length, 6 + add_length)
      )
    }

  }

  df <- dplyr::bind_rows(x)

  if(nrow(df) == 0) {
    df <- tibble::tibble(
      area_code = character(),
      region_code = character(),
      province_code = character(),
      city_mun_code = character()
    )
  }

  dplyr::distinct(df, area_code, .keep_all = T)

}



#' Title
#'
#' @param parquet_list
#'
#' @return
#' @export
#'
#' @examples

get_dataset_summary <- function(parquet_list, dictionary = NULL) {

  stat_refs <- c(
    "Max." = "Maximum",
    "Min." = "Minimum",
    "Mean" = "Mean",
    "1st Qu." = "First Quartile",
    "Median" = "Median",
    "3rd Qu." = "Third Quartile",
    "NA's" = "NA / Missing",
    "Sum" = "Sum",
    "Length" = "Frequency",
    "Class" = "Class",
    "Mode" = "Mode"
  )

  pq_names <- names(parquet_list)

  df_list <- list()

  for(i in seq_along(pq_names)) {

    pq_name <- pq_names[i]

    df <- parquet_list[[pq_name]] |>
      dplyr::collect()


    if(!is.null(dictionary)) {

      df_names <- names(df)

      for(i in seq_along(df_names)) {

        df_name <- df_names[i]

        vs <- dictionary |>
          dplyr::filter(variable_name == df_name)

        if(nrow(vs) == 0) next
        if(is.null(vs$valueset)) next
        if(is.null(vs$valueset[[1]])) next

        vs_i <- vs$valueset[[1]]

        df[[df_name]] <- factor(df[[df_name]], vs_i$value, vs_i$label)

      }
    }

    df_temp <- as.data.frame(summary(df)) |>
      janitor::clean_names() |>
      dplyr::filter(!is.na(freq)) |>
      dplyr::as_tibble() |>
      dplyr::transmute(
        variable_name = stringr::str_trim(var2) |>
          stringr::str_squish() |>
          as.character(),
        stat = purrr::map(freq, \(x) {
          y <- stringr::str_split(x, ':')[[1]]

          v1 <- stat_refs[stringr::str_trim(y[1])]
          v2 <- stringr::str_trim(y[2])

          if(!is.na(v1)) {
            if(v1 == "Frequency" | v1 == "NA / Missing") {
              v2 <- prettyNum(as.integer(v2), big.mark = ",")
            }
          } else {
            v1 <- stringr::str_trim(y[1])
          }

          tibble::tibble(
            stat = v1,
            value = v2
          )
        })
      ) |>
      tidyr::unnest(stat) |>
      dplyr::group_by(variable_name) |>
      tidyr::nest(.key = 'stats') |>
      dplyr::mutate(
        missing = purrr::map_dbl(variable_name, function(x) {
          y <- df[[x]]
          100 * (length(y[is.na(y)]) / length(y))
        }),
        count = purrr::map_int(variable_name, function(x) {
          length(df[[x]])
        }),
        distinct = purrr::map_int(variable_name, function(x) {
          length(unique(df[[x]]))
        })
      )

    if(!is.null(dictionary)) {

      df_temp <- df_temp |>
        dplyr::left_join(
          dictionary,
          by = 'variable_name'
        ) |>
        dplyr::mutate(
          extremes = purrr::map2(variable_name, type, function(x, y) {

            z <- df[[x]]
            z_u <- unique(z)
            cutoff <- 5
            z_length <- length(z_u)
            if(z_length < 5) {
              cutoff <- 2
            }

            if(is.na(y)) { return(list()) }

            if(y == 'i' | y == 'n' | y == 'd') {

              lower_5 <- sort(unique(z))[1:cutoff]
              upper_5 <- sort(rev(sort(unique(z)))[1:cutoff])

              list(
                lower_5 = lower_5,
                upper_5 = upper_5
              )

            } else if (y == 'D') {

              z <- as.POSIXct(z)

              lower_5 <- sort(unique(z))[1:cutoff]
              upper_5 <- sort(rev(sort(unique(z)))[1:cutoff])

              list(
                lower_5 = lower_5,
                upper_5 = upper_5
              )

            } else {
              list()
            }

          })
        ) |>
        dplyr::select(
          variable_name,
          label,
          type,
          valueset,
          missing,
          count,
          distinct,
          extremes,
          stats
        )
    }

    df_list[[pq_name]] <- df_temp


  }

  df_list

}
