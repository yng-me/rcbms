read_cbms_hp_data <- function(.path, .dictionary = NULL) {

  col_types <- NULL
  if(!is.null(.dictionary)) {

    if(!('types' %in% names(.dictionary)) && !('variable_length' %in% names(.dictionary))) {
      stop('Data dictionary is invalid.')
    }

    if(length(.dictionary$types) > 0) {
      col_types <- .dictionary$types
    }
  }

  df <- readr::read_delim(
      .path,
      delim = '\t',
      quote = "",
      progress = F,
      trim_ws = T,
      col_types = col_types,
      show_col_types = F
    ) |>
    clean_colnames() |>
    mutate(Barangay = t)

  return(df)
}
