#' Title
#'
#' @param .data
#' @param .x
#' @param .y
#' @param ...
#' @param .agg_levels
#' @param .preserve_order
#' @param .top_n
#' @param .config
#'
#' @return
#' @export
#'
#' @examples
#'
generate_tab <- function(
    .data,
    .x,
    .y = NULL,
    ...,
    .agg_levels = NULL,
    .preserve_order = F,
    .top_n = NULL,
    .config = getOption("rcbms.config")
) {

  if(is.null(.agg_levels)) {
    .agg_levels <- c("region", "province", "city_mun", "barangay")
  }

  tbl_all <- list()

  .data <- .data |>
    factor_cols({{.x}}) |>
    factor_cols({{.y}})


  for(i in seq_along(.agg_levels)) {

    agg <- .agg_levels[i]

    tbl_list <- .data |>
      tsg::generate_as_list(
        !!as.name(paste0(agg, "_geo")),
        {{.x}},
        {{.y}},
        ...,
        exclude_overall = FALSE,
        y_group_separator = "__"
      )

    tbl_list2 <- list()

    tbl_names <- names(tbl_list)
    tbl_names <- tbl_names[tbl_names != 'ALL']

    for(j in seq_along(tbl_names)) {

      tbl_name <- tbl_names[j]
      tbl2 <- tbl_list[[tbl_name]] |>
        dplyr::mutate(
          area_code = stringr::str_pad(tbl_name, width = 9, side = "right", pad = "0"),
          .before = 1
        ) |>
        dplyr::mutate(order = 1:dplyr::n(), .before = 2) |>
        dplyr::mutate(order = dplyr::if_else({{.x}} == "Total", NA_real_, order))

      if(!is.null(.top_n)) {
        tbl2 <- tbl2 |>
          dplyr::filter(order <= .top_n)
      }

      if(!.preserve_order) tbl2 <- tbl2 |> select(-order)

      tbl_list2[[tbl_name]] <- tbl2 |>
        dplyr::select(-dplyr::contains("cumulative"))
    }

    tbl_all[[i]] <- dplyr::bind_rows(tbl_list2) |>
      dplyr::mutate(level = agg, .before = 2)
  }

  tbl_all |>
    dplyr::bind_rows() |>
    dplyr::mutate(survey_round = .config$survey_round) |>
    dplyr::rename(label = {{.x}}) |>
    janitor::clean_names()

}


#' Title
#'
#' @param .data
#' @param .x
#' @param .y
#' @param .agg_levels
#'
#' @return
#' @export
#'
#' @examples
generate_tab_d2 <- function(.data, .x, .y, .agg_levels = NULL) {

  if(is.null(.agg_levels)) {
    .agg_levels <- c("region", "province", "city_mun", "barangay")
  }

  tbl_list <- list()

  for(i in seq_along(.agg_levels)) {

    agg_level <- .agg_levels[i]
    agg <- paste0(agg_level, "_geo")

    tbl_list[[i]] <- .data |>
      mutate(x = {{.x}}, y = {{.y}}) |>
      factor_cols({{.x}}) |>
      factor_cols({{.y}}) |>
      rename(
        area_code = !!as.name(agg),
        x_label = {{.x}},
        y_label = {{.y}}
      ) |>
      add_count(area_code, name = "total_hhm") |>
      add_count(area_code, y, name = "total") |>
      group_by(area_code, x, x_label, y, y_label, total_hhm, total) |>
      count(name = "count") |>
      head(5) |>
      ungroup() |>
      mutate(
        percent = 100 * (count / total),
        level = agg_level,
        survey_round = config$survey_round
      )
  }

  tbl_list[["overall"]] <- .data |>
    mutate(x = {{.x}}, y = {{.y}}) |>
    factor_cols({{.x}}) |>
    factor_cols({{.y}}) |>
    rename(
      x_label = {{.x}},
      y_label = {{.y}}
    ) |>
    add_count(name = "total_hhm") |>
    add_count(y, name = "total") |>
    group_by(x, x_label, y, y_label, total_hhm, total) |>
    count(name = "count") |>
    ungroup() |>
    mutate(
      area_code = "0",
      percent = 100 * (count / total),
      level = "overall",
      survey_round = config$survey_round
    )

  bind_rows(tbl_list) |>
    mutate(area_code = stringr::str_pad(area_code, width = 9, side = "right", pad = "0"))
}


#' Title
#'
#' @param .data
#' @param .x
#' @param .agg_levels
#' @param .top_n
#'
#' @return
#' @export
#'
#' @examples
generate_tab_d1 <- function(.data, .x, .agg_levels = NULL) {

  if(is.null(.agg_levels)) {
    .agg_levels <- c("region", "province", "city_mun", "barangay")
  }

  tbl_list <- list()

  for(i in seq_along(.agg_levels)) {

    agg_level <- .agg_levels[i]
    agg <- paste0(agg_level, "_geo")

    tbl_list[[i]] <- .data |>
      mutate(x = {{.x}}) |>
      factor_cols({{.x}}) |>
      rename(
        area_code = !!as.name(agg),
        x_label = {{.x}}
      ) |>
      add_count(area_code, name = "total_hhm") |>
      group_by(area_code, x, x_label, total_hhm) |>
      count(name = "count") |>
      ungroup() |>
      mutate(
        percent = 100 * (count / total_hhm),
        level = agg_level,
        survey_round = config$survey_round
      )
  }

  tbl_list[["overall"]] <- .data |>
    mutate(x = {{.x}}) |>
    factor_cols({{.x}}) |>
    rename(
      x_label = {{.x}}
    ) |>
    add_count(name = "total_hhm") |>
    group_by(x, x_label,  total_hhm) |>
    count(name = "count") |>
    ungroup() |>
    mutate(
      area_code = "0",
      percent = 100 * (count / total_hhm),
      level = "overall",
      survey_round = config$survey_round
    )

  bind_rows(tbl_list) |>
    mutate(area_code = stringr::str_pad(area_code, width = 9, side = "right", pad = "0"))
}
