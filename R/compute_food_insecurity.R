#' Title
#'
#' @param .data
#' @param ...
#' @param .type
#' @param .agg_levels
#' @param .config
#' @param .valueset
#' @param .extract_name_position
#'
#' @return
#' @export
#'
#' @examples
#'

compute_food_insecurity <- function(
  .data,
  ...,
  .type = 1,
  .agg_levels = NULL,
  .valueset = get_config("references")$valueset,
  .extract_name_position = 5,
  .config = getOption("rcbms.config")
) {

  if(is.null(.agg_levels)) {
    .agg_levels <- c("region", "province", "city_mun")
  }

  rm_colnames <- .data |> select(...) |> names()

  rm_by_item_all <- list()
  rm_by_score_all <- list()
  rm_by_prevalence_all <- list()

  for(i in seq_along(.agg_levels)) {

    rm_by_item <- list()
    rm_by_score <- list()
    rm_by_prevalence <- list()

    agg <- .agg_levels[i]
    agg_geo <- paste0(agg, "_geo")
    area_codes <- unique(.data[[agg_geo]])

    for(k in 1:length(area_codes)) {

      area_code <- area_codes[k]
      rm_data <- .data |>
        filter(!!as.name(agg_geo) == area_code) |>
        select(...)

      rm <- RM.weights::RM.w(rm_data)
      rm_prob <- RM.weights::equating.fun(rm)

      rm_item_b <- rm$b |> as_tibble_col('item_severity')
      rm_item_se.b <- rm$se.b |> as_tibble_col('item_severity_se')
      rm_item_infit <- rm$infit |> as_tibble_col('infit_stat')
      rm_item_se.infit <- rm$se.infit |> as_tibble_col('infit_stat_se')
      rm_item_outfit <- rm$outfit |> as_tibble_col('outfit_stat')

      rm_by_item[[area_code]] <- as_tibble_col(rm_colnames, 'name') |>
        add_column(rm_item_b) |>
        add_column(rm_item_se.b) |>
        add_column(rm_item_infit) |>
        add_column(rm_item_se.infit) |>
        add_column(rm_item_outfit) |>
        dplyr::mutate(
          area_code = stringr::str_pad(area_code, width = 9, side = "right", pad = "0")
        )

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

      rm_by_score[[area_code]] <- as_tibble_col(c(0:8), 'raw_score') |>
        mutate(raw_score = as.character(raw_score)) |>
        add_column(rm_score_a) |>
        add_column(rm_score_se.a) |>
        add_column(rm_score_prob) |>
        dplyr::mutate(
          area_code = stringr::str_pad(area_code, width = 9, side = "right", pad = "0")
        )

      rm_by_prevalence[[area_code]] <- tibble(
        prevalence_moderate_and_severe = 100 * rm_prob$prevs[[1]],
        prevalence_severe = 100 * rm_prob$prevs[[2]],
        reliability = rm$reliab,
        reliability_equal_weights = rm$reliab.fl
      ) |>
        dplyr::mutate(
          area_code = stringr::str_pad(area_code, width = 9, side = "right", pad = "0")
        )
    }

    rm_by_item_all[[i]] <- dplyr::bind_rows(rm_by_item) |> dplyr::mutate(level = agg)
    rm_by_score_all[[i]] <-  dplyr::bind_rows(rm_by_score) |> dplyr::mutate(level = agg)
    rm_by_prevalence_all[[i]] <- dplyr::bind_rows(rm_by_prevalence) |> dplyr::mutate(level = agg)

  }

  by_item <- dplyr::bind_rows(rm_by_item_all)

  if(!is.null(.valueset)) {

    .valueset <- .valueset |>
      dplyr::filter(name == "fies") |>
      dplyr::select(-dplyr::any_of("name")) |>
      dplyr::rename(name = value)

    if(grepl("\\d+", .valueset$name[1])) {
      .valueset |>
        dplyr::mutate(value = as.integer(value))
    }

    by_item <- by_item |>
      dplyr::mutate(
        name = toupper(
          stringr::str_sub(name, .extract_name_position, .extract_name_position)
        )
      ) |>
      dplyr::left_join(.valueset, by = "name") |>
      dplyr::rename(item = name)
  }


  v <- list(
    prevalence = dplyr::bind_rows(rm_by_prevalence_all) |>
      tidyr::pivot_longer(dplyr::any_of(
        c(
          "prevalence_moderate_and_severe",
          "prevalence_severe",
          "reliability",
          "reliability_equal_weights"
        )
      )) |>
      dplyr::mutate(
        label = dplyr::case_when(
          name == "prevalence_moderate_and_severe" ~ "Prevalence of moderate + severy food insecuriy",
          name == "prevalence_severe" ~ "Prevalence of severy food insecuriy",
          name == "reliability" ~ "Reliability of measure",
          name == "reliability_equal_weights" ~ "Reliability of measure (weighted)"
        )
      ),
    item = by_item,
    score = dplyr::bind_rows(rm_by_score_all)
  )

  return(
    v[[.type]] |>
      dplyr::mutate(survey_round = as.integer(.config$survey_round), .before = 1) |>
      dplyr::select(area_code, level, survey_round, everything())
  )
}
