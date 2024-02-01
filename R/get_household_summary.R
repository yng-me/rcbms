#' Title
#'
#' @param .data
#' @param .relation_to_hh_head
#' @param .nuclear_family
#' @param .agg_levels
#' @param .sex
#' @param .include_overall
#' @param .include_survey_round
#' @param .include_individuals
#' @param .config
#'
#' @return
#' @export
#'
#' @examples
#'

get_household_summary <- function(
  .data,
  .sex,
  .relation_to_hh_head,
  .nuclear_family,
  .agg_levels = NULL,
  .include_overall = T,
  .include_survey_round = T,
  .include_individuals = T,
  .config = getOption("rcbms.config")
) {

  hh_demog_list <- list()

  agg_labels <- c("region", "province", "city_mun", "barangay")
  if(is.null(.agg_levels)) {
    .agg_levels <- c(3, 4)
  }

  compute_hhm_summary <- function(.df, ...) {
    .df |>
      dplyr::select({{.sex}}, ...) |>
      dplyr::mutate({{.sex}} := factor({{.sex}}, c(1, 2), c("frequency_male", "frequency_female"))) |>
      dplyr::group_by(..., {{.sex}}) |>
      dplyr::count() |>
      tidyr::pivot_wider(
        names_from = {{.sex}},
        values_from = n,
        values_fill = 0,
        names_expand = T
      ) |>
      dplyr::mutate(
        percent_male = 100 * (frequency_male / (frequency_male + frequency_female)),
        percent_female = 100 * (frequency_female / (frequency_male + frequency_female)),
        sex_ratio = paste0(round((100 *(frequency_male / frequency_female)), 0), ":", 100)
      )
  }

  compute_hh_summary <- function(.df, ...) {

    .df |>
      dplyr::mutate(
        fam = dplyr::if_else({{.relation_to_hh_head}} %in% c(1:23), 1L, 0L),
        male = dplyr::if_else({{.sex}} == 1L, 1L, 0L),
        female = dplyr::if_else({{.sex}} == 2L, 1L, 0L)
      ) |>
      dplyr::group_by(..., case_id) |>
      dplyr::summarise(
        n = dplyr::n(),
        fam_s = sum(fam, na.rm = T),
        nuc_s = dplyr::n_distinct({{.nuclear_family}}, na.rm = T),
        .groups = 'drop_last'
      ) |>
      dplyr::summarise(
        total_hh = dplyr::n(),
        total_hhm = sum(n, na.rm = T),
        average_hh_size = round(mean(n, na.rm = T), 3),
        average_family_size = round(mean(fam_s, na.rm = T), 3),
        average_number_nuclear_family = round(mean(nuc_s, na.rm = T), 3),
        .groups = 'drop_last'
      )
  }

  for(i in seq_along(.agg_levels)) {

    level_i <- .agg_levels[i]
    agg_geo <- paste0(agg_labels[level_i], "_geo")
    agg_name <- paste0(agg_labels[level_i], "_agg")

    df <- .data |>
      compute_hh_summary(!!as.name(agg_geo), !!as.name(agg_name))

    if(.include_individuals) {
      df <- df |>
        dplyr::left_join(
          compute_hhm_summary(.data, !!as.name(agg_geo), !!as.name(agg_name)),
          by = c(agg_geo, agg_name)
        )
    }

    hh_demog_list[[i]] <- df |>
      rename(area_code = 1, area_name = 2) |>
      dplyr::mutate(area_code = stringr::str_pad(area_code, width = 9, side = "right", pad = "0")) |>
      dplyr::mutate(level = as.integer(level_i), .after = 2)
  }

  if(.include_overall) {

    df <- .data |> compute_hh_summary()

    if(.include_individuals) {
      df <- df |> dplyr::bind_cols(compute_hhm_summary(.data))
    }

    hh_demog_list[["overall"]] <- df |>
      dplyr::mutate(area_code = "0", area_name = ":GRAND_SUMMARY:", level = 0L, .before = 1) |>
      dplyr::mutate(area_code = stringr::str_pad(area_code, width = 9, side = "right", pad = "0"))

  }

  df_final <- hh_demog_list |> dplyr::bind_rows()

  if(.include_survey_round) {
    df_final <- df_final |>
      dplyr::mutate(survey_round = as.integer(.config$survey_round), .after = 3)
  }

  df_final

}
