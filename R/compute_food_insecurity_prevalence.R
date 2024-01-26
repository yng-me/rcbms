#' Title
#'
#' @param .data
#' @param ...
#' @param .results
#' @param .agg_levels
#'
#' @return
#' @export
#'
#' @examples
#'
#'

compute_food_insecurity_prevalence <- function(
  .data,
  ...,
  .results = 1,
  .agg_levels = NULL
) {

  if(is.null(.agg_levels)) {
    .agg_levels <- c("region", "province", "city_mun", "barangay")
  }

  rm_colnames <- .data |>
    select(...) |>
    names()

  rm_by_item <- list()
  rm_by_score <- list()
  rm_by_prevalence <- list()

  for(i in seq_along(.agg_levels)) {

    agg <- .agg_levels[i]
    agg_geo <- paste0(agg, "_geo")
    rm_area_codes <- unique(.data[[agg_geo]])

    for(k in 1:length(rm_area_codes)) {

      rm_area_code <- rm_area_codes[k]
      rm_data <- .data |>
        filter(!!as.name(agg_geo) == rm_area_code) |>
        select(...)

      rm <- RM.weights::RM.w(rm_data)
      rm_prob <- RM.weights::equating.fun(rm)

      # ===============================================================
      # by item
      rm_item_b <- rm$b |> as_tibble_col('item_severity')
      rm_item_se.b <- rm$se.b |> as_tibble_col('item_severity_se')
      rm_item_infit <- rm$infit |> as_tibble_col('infit_stat')
      rm_item_se.infit <- rm$se.infit |> as_tibble_col('infit_stat_se')
      rm_item_outfit <- rm$outfit |> as_tibble_col('outfit_stat')

      rm_by_item[[rm_area_code]] <- as_tibble_col(rm_colnames, 'item') |>
        add_column(rm_item_b) |>
        add_column(rm_item_se.b) |>
        add_column(rm_item_infit) |>
        add_column(rm_item_se.infit) |>
        add_column(rm_item_outfit)

      # ===============================================================
      # by score
      rm_score_a <- rm$a |> as_tibble_col('severity_raw_score')
      rm_score_se.a <- rm$se.a |> as_tibble_col('severity_raw_score_se')
      rm_score_wt.rs <- rm$wt.rs |> as_tibble_col('total_hh')

      rm_score_wt.rel.rs <- rm$wt.rel.rs |>
        as_tibble_col('percent') |>
        mutate(percent = percent * 100)

      rm_score_prob <- rm_score_wt.rs |>
        add_column(rm_score_wt.rel.rs) |>
        add_column(as_tibble(rm_prob$probs.rs)) |>
        mutate(
          mod_sev = percent * `FI_mod+`,
          sev = percent * FI_sev
        ) |>
        select(-starts_with("FI_"))

      rm_by_score[[rm_area_code]] <- as_tibble_col(c(0:8), 'raw_score') |>
        mutate(raw_score = as.character(raw_score)) |>
        add_column(rm_score_a) |>
        add_column(rm_score_se.a) |>
        add_column(rm_score_prob)

      rm_by_prevalence[[rm_area_code]] <- tibble(
        prevalence_moderarte_and_severe = 100 * rm_prob$prevs[[1]],
        prevalence_severe = 100 * rm_prob$prevs[[2]],
        reliability = rm$reliab,
        reliability_equal_weights = rm$reliab.fl
      )
    }

    rm_by_item <- bind_rows(rm_by_item) |>
      mutate(
        area_code = stringr::str_pad(rm_area_code, width = 9, side = "right", pad = "0"),
        level = agg,
        .before = 1
      )

    rm_by_score <- bind_rows(rm_by_score) |>
      mutate(
        area_code = stringr::str_pad(rm_area_code, width = 9, side = "right", pad = "0"),
        level = agg,
        .before = 1
      )

    rm_by_prevalence <- bind_rows(rm_by_prevalence) |>
      mutate(
        area_code = stringr::str_pad(rm_area_code, width = 9, side = "right", pad = "0"),
        level = agg,
        .before = 1
      )
  }

  v <- list(
    prevalence = rm_by_prevalence,
    item = rm_by_item,
    score = rm_by_score
  )
  return(v[[.results]])
}
