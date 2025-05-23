#' Title
#'
#' @param .data
#' @param ...
#' @param .agg_levels
#' @param .total_by_cols
#' @param .keep_col_names
#' @param .sort
#' @param .multiple_response
#' @param .valueset
#' @param .extract_name_position
#' @param .config
#'
#' @return
#' @export
#'
#' @examples
generate_tabs <- function(
  .data,
  ...,
  .agg_levels = NULL,
  .total_by_cols = FALSE,
  .invert = FALSE,
  .complete = TRUE,
  .keep_col_names = FALSE,
  .multiple_response = FALSE,
  .valueset = NULL,
  .extract_name_position = 5,
  .sort = TRUE,
  .include_overall = FALSE,
  .config = getOption("rcbms.config")
) {
  agg_labels <- c("region", "province", "city_mun", "barangay")

  if (is.null(.agg_levels)) {
    .agg_levels <- c(3, 4)
  }

  if (.multiple_response) {
    return(
      .data |>
        generate_tab_multiple(
          ...,
          .agg_levels = .agg_levels,
          .valueset = .valueset,
          .extract_name_position = .extract_name_position,
          .include_overall = .include_overall,
          .config = .config
        )
    )
  }

  tbl_list <- list()
  cols <- sapply(substitute(list(...))[-1], deparse)
  col_x <- cols[1]
  col_y <- cols[2]

  for (i in seq_along(.agg_levels)) {
    level_i <- .agg_levels[i]
    agg_level <- agg_labels[level_i]
    agg <- paste0(agg_level, "_geo")

    tbl_list[[i]] <- .data |>
      dplyr::rename(area_code = !!as.name(agg)) |>
      generate_tab(
        area_code,
        .cols = cols,
        .total_by_cols = .total_by_cols,
        .sort = .sort
      ) |>
      factor_cols(..., .complete = .complete) |>
      dplyr::mutate(level = as.integer(level_i))
  }

  if(.include_overall) {
    tbl_list[["overall"]] <- .data |>
      dplyr::add_count(name = "total") |>
      generate_tab(
        .cols = cols,
        .total_by_cols = .total_by_cols,
        .sort = .sort
      ) |>
      factor_cols(..., .complete = .complete) |>
      dplyr::mutate(area_code = "0", level = 0L)
  }

  tbl_list <- bind_rows(tbl_list)

  if (.keep_col_names) {
    tbl_list <- tbl_list |>
      dplyr::rename_with(
        ~ stringr::str_remove(., "^[a-r]\\d{2}_"),
        dplyr::matches("^[a-r]\\d{2}_.*")
      )
  } else {
    tbl_list <- tbl_list |>
      dplyr::rename_with(
        ~ paste0("x", stringr::str_remove(., col_x)),
        dplyr::matches(paste0("^", col_x))
      )

    if (length(cols) == 2) {
      tbl_list <- tbl_list |>
        dplyr::rename_with(
          ~ paste0("y", stringr::str_remove(., col_y)),
          dplyr::matches(paste0("^", col_y))
        )
    }
  }

  width_geo_pad <- 9
  if(.config$survey_round == '2024') width_geo_pad <- 10

  tbl <- tbl_list |>
    dplyr::mutate(
      area_code = stringr::str_pad(
        area_code,
        width = width_geo_pad,
        side = "right",
        pad = "0"
      ),
      survey_round = as.integer(.config$survey_round)
    )

  if(!.include_overall) {
    tbl <- tbl |> filter(level != 0)
  }

  if(.invert & .total_by_cols & length(cols) > 1) {

    tbl <- tbl |>
      dplyr::rename(xx = x) |>
      dplyr::rename(x = y) |>
      dplyr::rename(y = xx)

    if('x_fct' %in% names(tbl) & 'y_fct' %in% names(tbl)) {
      tbl <- tbl |>
        dplyr::rename(xx_fct = x_fct) |>
        dplyr::rename(x_fct = y_fct) |>
        dplyr::rename(y_fct = xx_fct)
    } else if ('x_fct' %in% names(tbl) & !('y_fct' %in% names(tbl))) {
      tbl <- tbl |>
        dplyr::rename(y_fct = x_fct)

    } else if ('y_fct' %in% names(tbl) & !('x_fct' %in% names(tbl))) {
      tbl <- tbl |>
        dplyr::rename(x_fct = y_fct)
    }
  }


  tbl |>
    dplyr::select(
      area_code,
      survey_round,
      level,
      dplyr::contains("total"),
      dplyr::contains(c("x", "y")),
      dplyr::everything()
    )
}



generate_tab <- function(.data, ..., .cols, .total_by_cols = FALSE, .sort = TRUE) {
  if (.total_by_cols & length(.cols) > 1) {
    .data |>
      dplyr::add_count(..., name = "total") |>
      dplyr::add_count(..., !!as.name(.cols[2]), name = "total_y") |>
      dplyr::group_by(..., total, total_y, dplyr::pick(dplyr::any_of(.cols))) |>
      dplyr::count(name = "count", sort = .sort) |>
      dplyr::mutate(percent = 100 * (count / total_y))

  } else {
    .data |>
      dplyr::add_count(..., name = "total") |>
      dplyr::group_by(..., total, dplyr::pick(dplyr::any_of(.cols))) |>
      dplyr::count(name = "count", sort = .sort) |>
      dplyr::mutate(percent = 100 * (count / total))
  }
}


generate_tab_multiple <- function(
  .data,
  ...,
  .agg_levels = NULL,
  .valueset = NULL,
  .extract_name_position = 5,
  .include_overall = FALSE,
  .config = getOption("rcbms.config")
) {
  agg_labels <- c("region", "province", "city_mun", "barangay")

  if (is.null(.agg_levels)) {
    .agg_levels <- c(3, 4)
  }

  # if(rlang::dots_n(...) == 1) {
  #
  #   multi_type_df <- multi_type_df |>
  #     dplyr::select(...) |>
  #     dplyr::select(1) |>
  #     dplyr::pull(1)
  #
  #   if(!(grepl("\\d+", multi_type_df[1]))) {
  #
  #     .data <- .data |>
  #       dplyr::mutate(type = toupper(stringr::str_trim(!!as.name(y)))) |>
  #       dplyr::collect() |>
  #       dplyr::mutate(type = dplyr::if_else(type == '', NA_character_, type)) |>
  #       dplyr::mutate(type = strsplit(type, split = '')) |>
  #       tidyr::unnest(type) |>
  #       dplyr::filter(!is.na(type), grepl('^[A-Z]$', type)) |>
  #       dplyr::group_by(case_id, type) |>
  #       dplyr::count() |>
  #       dplyr::ungroup() |>
  #       dplyr::arrange(type) |>
  #       tidyr::pivot_wider(
  #         names_from = type,
  #         values_from = n,
  #         values_fill = 0,
  #         names_sort = F
  #       )
  #   }
  # }
  #
  tbl_list <- list()

  for (i in seq_along(.agg_levels)) {
    level_i <- .agg_levels[i]
    agg_level <- agg_labels[level_i]
    agg <- paste0(agg_level, "_geo")

    tbl_list[[i]] <- .data |>
      dplyr::rename(area_code = !!as.name(agg)) |>
      dplyr::add_count(area_code, name = "total") |>
      dplyr::mutate_at(dplyr::vars(...), ~ dplyr::if_else(. == 1, 1L, 0L, 0L)) |>
      dplyr::group_by(area_code, total) |>
      dplyr::summarise_at(dplyr::vars(...), ~ sum(., na.rm = TRUE)) |>
      dplyr::mutate(level = as.integer(level_i))
  }

  tbl_list[["overall"]] <- .data |>
    dplyr::add_count(name = "total") |>
    dplyr::mutate_at(dplyr::vars(...), ~ dplyr::if_else(. == 1, 1L, 0L, 0L)) |>
    dplyr::group_by(total) |>
    dplyr::summarise_at(dplyr::vars(...), ~ sum(., na.rm = TRUE)) |>
    dplyr::mutate(area_code = "0", level = 0L)

  tbl <- tbl_list |>
    dplyr::bind_rows() |>
    tidyr::pivot_longer(...) |>
    dplyr::rename(count = value, x = name) |>
    dplyr::mutate(
      percent = 100 * (count / total),
      x = toupper(stringr::str_sub(x, .extract_name_position, .extract_name_position))
    )

  if (!is.null(.valueset)) {
    if (!("value" %in% names(.valueset) & "label" %in% names(.valueset))) {
      return(tbl)
    }

    if (grepl("\\d+", .valueset$value[1])) {
      .valueset |>
        dplyr::mutate(value = as.integer(value))
    }

    tbl <- tbl |>
      dplyr::mutate(value = x) |>
      dplyr::left_join(.valueset, by = "value") |>
      dplyr::rename(x_fct = label) |>
      dplyr::select(-c(value, name))
  }

  width_geo_pad <- 9
  if(.config$survey_round == '2024') width_geo_pad <- 10

  tbl <- tbl |>
    dplyr::mutate(
      area_code = stringr::str_pad(
        area_code,
        width = width_geo_pad,
        side = "right",
        pad = "0"
      ),
      survey_round = as.integer(.config$survey_round)
    ) |>
    dplyr::select(
      area_code,
      level,
      survey_round,
      total,
      x,
      dplyr::matches("_fct$"),
      everything()
    )

  if(!.include_overall) {
    tbl <- tbl |> filter(level != 0)
  }

  return(tbl)
}



#' Title
#'
#' @param .data
#' @param .col
#' @param ...
#' @param .col_value
#' @param .agg_levels
#'
#' @return
#' @export
#'
#' @examples
generate_tabs_alt <- function(.data, .col, ..., .col_value = 1, .agg_levels = NULL) {
  agg_labels <- c("region", "province", "city_mun", "barangay")

  cols <- sapply(substitute(list(...))[-1], deparse)

  if (is.null(.agg_levels)) {
    .agg_levels <- c(3, 4)
  }

  tbl_list <- list()

  for (i in seq_along(.agg_levels)) {
    level_i <- .agg_levels[i]
    agg_name <- agg_labels[level_i]
    agg_geo <- paste0(agg_name, "_geo")

    tbl_list[[i]] <- .data |>
      dplyr::filter({{ .col }} == .col_value) |>
      dplyr::mutate(area_code = !!as.name(agg_geo)) |>
      dplyr::group_by(area_code, ...) |>
      dplyr::count(name = "total_qualified") |>
      dplyr::left_join(
        .data |>
          dplyr::mutate(area_code = !!as.name(agg_geo)) |>
          dplyr::group_by(area_code, ...) |>
          dplyr::count(name = "total"),
        by = c("area_code", cols)
      ) |>
      dplyr::mutate(
        level = as.integer(level_i),
        percent = 100 * (total_qualified / total)
      )
  }

  tbl_list[["overall"]] <- .data |>
    dplyr::filter({{ .col }} == .col_value) |>
    dplyr::group_by(...) |>
    dplyr::count(name = "total_qualified") |>
    dplyr::left_join(
      .data |>
        dplyr::group_by(...) |>
        dplyr::count(name = "total"),
      by = cols
    ) |>
    dplyr::mutate(
      area_code = "0",
      level = 0L,
      percent = 100 * (total_qualified / total)
    )

  width_geo_pad <- 9
  if(.config$survey_round == '2024') width_geo_pad <- 10

  dplyr::bind_rows(tbl_list) |>
    dplyr::mutate(
      survey_round = as.integer(config$survey_round),
      area_code = stringr::str_pad(
        area_code,
        width = width_geo_pad,
        pad = "0",
        side = "right"
      )
    ) |>
    dplyr::select(
      area_code,
      survey_round,
      level,
      dplyr::everything()
    )
}
