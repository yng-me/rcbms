#' Title
#'
#' @param .data
#' @param .relation_to_hh_head
#' @param .nuclear_family
#' @param .agg_levels
#'
#' @return
#' @export
#'
#' @examples
#'

get_household_summary <- function(
  .data,
  .relation_to_hh_head,
  .nuclear_family,
  .agg_levels = NULL
) {

  hh_demog_list <- list()

  if(is.null(.agg_levels)) {
    .agg_levels <- c("region", "province", "city_mun", "barangay")
  }

  compute_hhm_summary <- function(.df, ...) {
    .df |>
      dplyr::select(sex, ...) |>
      dplyr::mutate(sex = factor(sex, c(1, 2), c("male_count", "female_count"))) |>
      dplyr::group_by(..., sex) |>
      dplyr::count() |>
      tidyr::pivot_wider(
        names_from = sex,
        values_from = n,
        values_fill = 0L
      ) |>
      dplyr::mutate(
        male_percent = 100 * (male_count / (male_count + female_count)),
        female_percent = 100 * (female_count / (male_count + female_count)),
        sex_ratio = paste0(round((100 *(male_count / female_count)), 0), ":", 100)
      )
  }
  compute_hh_summary <- function(.df, ...) {

    .df |>
      dplyr::mutate(
        fam = dplyr::if_else({{.relation_to_hh_head}} %in% c(1:23), 1L, 0L),
        male = dplyr::if_else(sex == 1L, 1L, 0L),
        female = dplyr::if_else(sex == 2L, 1L, 0L)
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

    agg_geo <- paste0(.agg_levels[i], "_geo")
    agg_name <- paste0(.agg_levels[i], "_agg")

    hh_demog_list[[i]] <- .data |>
      compute_hh_summary(!!as.name(agg_geo), !!as.name(agg_name)) |>
      dplyr::left_join(
        compute_hhm_summary(.data, !!as.name(agg_geo), !!as.name(agg_name)),
        by = c(agg_geo, agg_name)
      ) |>
      dplyr::rename(area_code = 1, area_name = 2) |>
      dplyr::mutate(area_code = stringr::str_pad(area_code, width = 9, side = "right", pad = "0")) |>
      dplyr::mutate(level = .agg_levels[i], .after = 2)
  }

  hh_demog_list[["overall"]] <- .data |>
    compute_hh_summary() |>
    dplyr::bind_cols(compute_hhm_summary(.data)) |>
    dplyr::mutate(area_code = "0", area_name = ":GRAND_SUMMARY:", level = "overall", .before = 1) |>
    dplyr::mutate(area_code = stringr::str_pad(area_code, width = 9, side = "right", pad = "0"))

  print(names(hh_demog_list[["overall"]]))

  do.call("rbind", hh_demog_list)
}
