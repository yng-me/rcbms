#' Title
#'
#' @param .data
#' @param ...
#' @param .ref_vs
#' @param .agg_levels
#'
#' @return
#' @export
#'
#' @examples
#'
generate_tab_multiple <- function(.data, ..., .ref_vs = NULL, .agg_levels = NULL) {

  if(is.null(.agg_levels)) {
    .agg_levels <- c("region", "province", "city_mun", "barangay")
  }

  df_list <- list()

  for(i in seq_along(.agg_levels)) {
    agg <- .agg_levels[i]

    df_list[[i]] <- .data |>
      tsg::generate_multiple_response(
        !!as.name(paste0(agg, "_geo")),
        ...,
        y_group_separator = "_"
      ) |>
      janitor::clean_names() |>
      filter(!!as.name(paste0(agg, "_geo")) != "Total") |>
      pivot_longer(-c(!!as.name(paste0(agg, "_geo")), total)) |>
      separate(name, into = c("label", "item"), sep = "_") |>
      pivot_wider(
        names_from = label,
        values_from = value
      ) |>
      mutate(value = toupper(item)) |>
      left_join(
        references$valueset |>
          filter(name == .ref_vs) |>
          select(value, label),
        by =  "value"
      ) |>
      mutate(
        area_code = stringr::str_pad(!!as.name(paste0(agg, "_geo")), width = 9, side = "right", pad = "0"),
        survey_round = config$survey_round,
        level = agg,
      ) |>
      select(-c(item, !!as.name(paste0(agg, "_geo")))) |>
      select(
        area_code,
        level,
        item = value,
        total_hh = total,
        label,
        everything()
      )
  }

  df_list |> bind_rows()

}
