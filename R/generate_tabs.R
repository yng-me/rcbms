#' Title
#'
#' @param .data
#' @param ...
#' @param .agg_levels
#' @param .total_by_cols
#' @param .keep_col_names
#' @param .sort
#'
#' @return
#' @export
#'
#' @examples
#'

generate_tabs <- function(
  .data,
  ...,
  .agg_levels = NULL,
  .total_by_cols = FALSE,
  .keep_col_names = FALSE,
  .multiple_response = FALSE,
  .valueset = NULL,
  .extract_name_position = 5,
  .sort = TRUE,
  .config = getOption("rcbms.config")
) {

  if(is.null(.agg_levels)) {
    .agg_levels <- c("city_mun", "barangay")
  }

  if(.multiple_response) {
    return(
      .data |>
        generate_tab_multiple(
          ...,
          .agg_levels = .agg_levels,
          .valueset = .valueset,
          .extract_name_position = .extract_name_position
        )
    )
  }

  tbl_list <- list()
  cols <- sapply(substitute(list(...))[-1], deparse)
  col_x <- cols[1]
  col_y <- cols[2]

  for(i in seq_along(.agg_levels)) {

    agg_level <- .agg_levels[i]
    agg <- paste0(agg_level, "_geo")

    tbl_list[[i]] <- .data |>
      dplyr::rename(area_code = !!as.name(agg)) |>
      generate_tab(
        area_code,
        .cols = cols,
        .total_by_cols = .total_by_cols,
        .sort = .sort
      ) |>
      dplyr::mutate(level = agg_level)

  }

  tbl_all <- .data |>
    dplyr::add_count(name = "total_hhm") |>
    generate_tab(
      .cols = cols,
      .total_by_cols = .total_by_cols,
      .sort = .sort
    ) |>
    factor_cols(...) |>
    dplyr::mutate(area_code = "0", level = "overall")

  df <- tbl_list |>
    bind_rows() |>
    factor_cols(...) |>
    bind_rows(tbl_all)

  if(.keep_col_names) {
    df <- df |>
      dplyr::rename_with(
        ~ stringr::str_remove(., "^[a-r]\\d{2}_"),
        dplyr::matches("^[a-r]\\d{2}_.*")
      )
  } else {
    df <- df |>
      dplyr::rename_with(
        ~ paste0("x", stringr::str_remove(., col_x)),
        dplyr::matches(paste0("^", col_x))
      )

    if(length(cols) == 2) {
      df <- df |>
        dplyr::rename_with(
          ~ paste0("y", stringr::str_remove(., col_y)),
          dplyr::matches(paste0("^", col_y))
        )
    }
  }

  df |>
    dplyr::mutate(
      area_code = stringr::str_pad(
        area_code,
        width = 9,
        side = "right",
        pad = "0"
      ),
      survey_round = .config$survey_round
    ) |>
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

  tbl <- .data |>
    dplyr::add_count(..., name = "total_hhm")

  if(.total_by_cols & length(cols) > 1) {

    tbl <- tbl |>
      dplyr::add_count(..., !!as.name(.cols[2]), name = "total") |>
      dplyr::group_by(..., total_hhm, total, dplyr::pick(dplyr::any_of(.cols))) |>
      dplyr::count(name = "count",  sort = .sort)  |>
      dplyr::mutate(percent = 100 * (count / total))

  } else {
    tbl <- tbl |>
      dplyr::group_by(..., total_hhm, dplyr::pick(dplyr::any_of(.cols))) |>
      dplyr::count(name = "count", sort = .sort) |>
      dplyr::mutate(percent = 100 * (count / total_hhm))
  }

  tbl

}


generate_tab_multiple <- function(
  .data,
  ...,
  .agg_levels = NULL,
  .valueset = NULL,
  .extract_name_position = 5
) {

  if(is.null(.agg_levels)) {
    .agg_levels <- c("region", "province", "city_mun", "barangay")
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

  for(i in seq_along(.agg_levels)) {

    agg_level <- .agg_levels[i]
    agg <- paste0(agg_level, "_geo")

    tbl_list[[i]] <- .data |>
      dplyr::rename(area_code = !!as.name(agg)) |>
      dplyr::add_count(area_code, name = "total") |>
      dplyr::mutate_at(dplyr::vars(...), ~ dplyr::if_else(. == 1, 1L, 0L, 0L)) |>
      dplyr::group_by(area_code, total) |>
      dplyr::summarise_at(dplyr::vars(...), ~ sum(., na.rm = TRUE)) |>
      dplyr::mutate(level = agg_level)
  }

  tbl_list[["overall"]] <- .data |>
    dplyr::add_count(name = "total") |>
    dplyr::mutate_at(dplyr::vars(...), ~ dplyr::if_else(. == 1, 1L, 0L, 0L)) |>
    dplyr::group_by(total) |>
    dplyr::summarise_at(dplyr::vars(...), ~ sum(., na.rm = TRUE)) |>
    dplyr::mutate(area_code = "0", level = "overall")

  tbl <- tbl_list |>
    dplyr::bind_rows() |>
    tidyr::pivot_longer(...) |>
    dplyr::rename(count = value, x = name) |>
    dplyr::mutate(
      percent = 100 * (count / total),
      x = toupper(stringr::str_sub(x, .extract_name_position, .extract_name_position))
    )

  if(!is.null(.valueset)) {

    if(!("value" %in% names(.valueset) & "label" %in% names(.valueset))) {
      return(tbl)
    }

    if(grepl("\\d+", .valueset$value[1])) {
      .valueset |>
        dplyr::mutate(value = as.integer(value))
    }

    tbl <- tbl |>
      dplyr::mutate(value = x) |>
      dplyr::left_join(.valueset, by = "value") |>
      dplyr::rename(x_fct = label) |>
      dplyr::select(-c(value, name))

  }

  tbl |>
    dplyr::mutate(
      area_code = stringr::str_pad(
        area_code,
        width = 9,
        side = "right",
        pad = "0"
      ),
      survey_round = .config$survey_round
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
}
