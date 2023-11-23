create_case_id <- function(.data, filter_completed = T) {

  if(!('case_id' %in% names(.data))) {

    df <- .data %>%
      collect() %>%
      mutate(
        case_id = paste0(
          str_pad(region_code, width = 2, pad = '0'),
          str_pad(province_code, width = 2, pad = '0'),
          str_pad(city_mun_code, width = 2, pad = '0'),
          str_pad(barangay_code, width = 3, pad = '0'),
          str_pad(ean, width = 6, pad = '0'),
          str_pad(bsn, width = 4, pad = '0'),
          str_pad(husn, width = 4, pad = '0'),
          str_pad(hsn, width = 4, pad = '0')
        )
      ) %>%
      select(case_id, everything())

    if(exists('completed_cases') & filter_completed) {
      df <- df %>%
        filter(case_id %in% completed_cases)
    }

    return(df)

  } else {

    return(.data %>% collect())

  }

}

create_line_number_id <- function(.data, join_with = NULL, filter_completed = T) {
  if(!('case_id' %in% names(.data))) {
    .data <- .data %>%
      create_case_id(filter_completed)
  }
  df <- .data %>%
    mutate(line_number_id = if_else(
      !is.na(line_number),
      paste0(case_id, str_pad(as.integer(line_number), width = 2, pad = '0')),
      NA_character_)
    )

  if(!is.null(join_with)) {
    df <- df %>%
      left_join(join_with, by = 'line_number_id')
  }
  return(df %>% select(case_id, line_number_id, everything()))
}


create_barangay_geo <- function(.data) {
  .data %>%
    mutate(
      barangay_geo = paste0(
        str_pad(as.character(region_code), width = 2, pad = '0'),
        str_pad(as.character(province_code), width = 2, pad = '0'),
        str_pad(as.character(city_mun_code), width = 2, pad = '0'),
        str_pad(as.character(barangay_code), width = 3, pad = '0')
      )
    )
}


create_uid <- function(format) {
  concat <- c()
  for(i in 1:nrow(format)) {
    append <- ''
    if(i < nrow(format)) append <- ', '
    concat <- paste0(
      concat,
      paste0(
        "sprintf('%0",
        format$digit[i],
        "d', as.integer(",
        format$var_name[i],
        "))",
        append
      )
    )
  }

  return(paste0("paste0(", concat, ")"))

}
