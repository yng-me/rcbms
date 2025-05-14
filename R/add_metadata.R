#' Add metadata
#'
#' @param .data
#' @param .dictionary
#' @param .valueset
#'
#' @return
#' @export
#'
#' @examples
#'
add_metadata <- function(.data, .dictionary, .valueset) {

  if(is.null(.dictionary) & is.null(.valueset)) return(.data)

  validate_required_cols(
    .dictionary,
    c("variable_name", "variable_name_new", "label")
  )

  validate_required_cols(.valueset, c('name', 'value', 'label'))

  df_name <- names(.data)

  .dictionary <- .dictionary |>
    convert_to_na() |>
    dplyr::distinct(variable_name, .keep_all = T) |>
    dplyr::mutate(
      variable = dplyr::if_else(
        is.na(variable_name_new),
        stringr::str_trim(variable_name),
        stringr::str_trim(variable_name_new)
      )
    ) |>
    dplyr::select(variable, label, dplyr::any_of(c('valueset', 'item', 'sub_item')), type) |>
    dplyr::distinct(.keep_all = T) |>
    dplyr::filter(variable %in% df_name, !is.na(label))

  for (i in seq_along(.dictionary$variable)) {
    v <- .dictionary$variable[i]
    attr(.data[[v]], "label") <- as.character(.dictionary$label[i])

    if('item' %in% names(.dictionary)) {
      item <- .dictionary$item[i]
      if (!is.na(item)) {
        attr(.data[[v]], "item") <- as.character(item)
      }
    }

    if('sub_item' %in% names(.dictionary)) {
      sub_item <- .dictionary$sub_item[i]
      if (!is.na(sub_item)) {
        attr(.data[[v]], "sub_item") <- as.character(sub_item)
      }
    }

    data_type <- .dictionary$type[i]
    if (!is.na(data_type)) {
      attr(.data[[v]], "type") <- as.character(data_type)
    }
  }

  if ("barangay_geo" %in% df_name) {
    attr(.data$barangay_geo, "label") <- "Barangay Geo ID"
  }

  if ("valueset" %in% names(.dictionary)) {
    for (j in seq_along(.dictionary$variable)) {
      vs <- .valueset |>
        dplyr::collect() |>
        dplyr::filter(name == .dictionary$valueset[j]) |>
        dplyr::select(-name)

      if (nrow(vs) > 0) {
        if (grepl("\\d+", vs$value[1])) {
          vs <- vs |>
            dplyr::mutate(value = as.integer(value))
        }

        attr(.data[[.dictionary$variable[j]]], "valueset") <- vs
      }
    }
  }

  return(.data)
}



#' Title
#'
#' @param .data
#' @param .age_prefix
#'
#' @return
#' @export
#'
#' @examples
#'
add_extra_meta <- function(.data, .age_prefix) {

  age_group_five_year <- paste0(.age_prefix, "_age_group_five_year")

  if(age_group_five_year %in% names(.data)) {

    attr(.data[[age_group_five_year]], "type") <- "i"
    attr(.data[[age_group_five_year]], "item") <- toupper(.age_prefix)
    attr(.data[[age_group_five_year]], "label") <- "5-year age group"
    attr(.data[[age_group_five_year]], "valueset") <- data.frame(
      value = 1L:21L,
      label = c(
        "< 5",
        "5-9",
        "10-14",
        "15-19",
        "20-24",
        "25-29",
        "30-34",
        "35-39",
        "40-44",
        "45-49",
        "50-54",
        "55-59",
        "60-64",
        "65-69",
        "70-74",
        "75-79",
        "80-84",
        "85-89",
        "90-94",
        "95-99",
        "100+"
      )
    )
  }

  age_group_hh_head <- paste0(.age_prefix, "_age_group_hh_head")

  if(age_group_hh_head %in% names(.data)) {
    attr(.data[[age_group_hh_head]], "type") <- "i"
    attr(.data[[age_group_hh_head]], "item") <- toupper(.age_prefix)
    attr(.data[[age_group_hh_head]], "label") <- "15-year age group"
    attr(.data[[age_group_hh_head]], "valueset") <- data.frame(
      value = 1L:7L,
      label = c(
        "Less than 15",
        "15-24",
        "25-34",
        "35-44",
        "45-54",
        "55-64",
        "65 and over"
      )
    )
  }


  age_group_fertility <- paste0(.age_prefix, "_age_group_fertility")

  if(age_group_fertility %in% names(.data)) {

    attr(.data[[age_group_fertility]], "type") <- "i"
    attr(.data[[age_group_fertility]], "item") <- toupper(.age_prefix)
    attr(.data[[age_group_fertility]], "label") <- "Age group (fertility)"
    attr(.data[[age_group_fertility]], "valueset") <- data.frame(
      value = 1L:9L,
      label = c(
        "Less than 15",
        "15-19",
        "20-24",
        "25-29",
        "30-34",
        "35-39",
        "40-44",
        "45-49",
        "50 and over"
      )
    )
  }

  age_group_schooling <- paste0(.age_prefix, "_age_group_schooling")

  if(age_group_schooling %in% names(.data)) {

    attr(.data[[age_group_schooling]], "type") <- "i"
    attr(.data[[age_group_schooling]], "item") <- toupper(.age_prefix)
    attr(.data[[age_group_schooling]], "label") <- "Age group (schooling)"
    attr(.data[[age_group_schooling]], "valueset") <- data.frame(
      value = 1L:7L,
      label = c(
        "3-4 years old",
        "5 years old",
        "6-11 years old",
        "12-15 years old",
        "16-17 years old",
        "18-20 years old",
        "21-24 years old"
      )
    )
  }

  if('region' %in% names(.data)) {
    attr(.data$region, 'label') <- 'Region'
    attr(.data$region, 'type') <- 'c'
  }

  if('province' %in% names(.data)) {
    attr(.data$province, 'label') <- 'Province/HUC'
    attr(.data$province, 'type') <- 'c'
  }

  if('city_mun' %in% names(.data)) {
    attr(.data$city_mun, 'label') <- 'City/Municipality'
    attr(.data$city_mun, 'type') <- 'c'
  }

  if('barangay' %in% names(.data)) {
    attr(.data$barangay, 'label') <- 'Barangay'
    attr(.data$barangay, 'type') <- 'c'
  }

  if('barangay_geo' %in% names(.data)) {
    attr(.data$barangay_geo, 'label') <- 'Barangay Geo ID'
    attr(.data$barangay_geo, 'type') <- 'nc'
  }

  return(.data)

}

