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

  agg_labels <- c("region", "province", "city_mun", "barangay")

  if(is.null(.agg_levels)) {
    .agg_levels <- c(3, 4)
  }

  rm_by_item_all <- list()
  rm_by_score_all <- list()
  rm_by_prevalence_all <- list()

  for(i in seq_along(.agg_levels)) {

    rm_by_item <- list()
    rm_by_score <- list()
    rm_by_prevalence <- list()

    level_i <- .agg_levels[i]
    agg <- agg_labels[level_i]
    agg_geo <- paste0(agg, "_geo")
    area_codes <- unique(.data[[agg_geo]])

    for(k in seq_along(area_codes)) {

      rm <- .data |>
        filter(!!as.name(agg_geo) == area_codes[k]) |>
        select(...) |>
        compute_food_insecurity_prevalence()

      rm_by_item[[k]] <- rm$item |>
        dplyr::mutate(area_code = stringr::str_pad(area_codes[k], width = 9, side = "right", pad = "0"))

      rm_by_score[[k]] <- rm$score |>
        dplyr::mutate(area_code = stringr::str_pad(area_codes[k], width = 9, side = "right", pad = "0"))

      rm_by_prevalence[[k]] <- rm$prevalence |>
        dplyr::mutate(area_code = stringr::str_pad(area_codes[k], width = 9, side = "right", pad = "0"))
    }

    rm_by_item_all[[i]] <- dplyr::bind_rows(rm_by_item) |>
      dplyr::mutate(level = as.integer(level_i))

    rm_by_score_all[[i]] <-  dplyr::bind_rows(rm_by_score) |>
      dplyr::mutate(level = as.integer(level_i))

    rm_by_prevalence_all[[i]] <- dplyr::bind_rows(rm_by_prevalence) |>
      dplyr::mutate(level = as.integer(level_i))

  }

  rm_all <- .data |>
    dplyr::select(...) |>
    compute_food_insecurity_prevalence()

  rm_by_prevalence_all$overall <- rm_all$prevalence |>
    dplyr::mutate(
      area_code = stringr::str_pad("0", width = 9, side = "right", pad = "0"),
      level = 0L
    )

  rm_by_score_all$overall <- rm_all$score |>
    dplyr::mutate(
      area_code = stringr::str_pad("0", width = 9, side = "right", pad = "0"),
      level = 0L
    )

  rm_by_item_all$overall <- rm_all$item |>
    dplyr::mutate(
      area_code = stringr::str_pad("0", width = 9, side = "right", pad = "0"),
      level = 0L
    )

  by_item <- dplyr::bind_rows(rm_by_item_all)

  if(!is.null(.valueset)) {

    .valueset <- .valueset |>
      dplyr::filter(name == "fies") |>
      dplyr::select(-name) |>
      dplyr::rename(name = value)

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
    prevalence = dplyr::bind_rows(rm_by_prevalence_all),
    item = by_item,
    score = dplyr::bind_rows(rm_by_score_all)
  )


  return(
    v[[.type]] |>
      dplyr::mutate(survey_round = as.integer(.config$survey_round), .before = 1) |>
      dplyr::select(area_code, level, survey_round, everything())
  )
}


compute_food_insecurity_prevalence <- function(.data) {

  rm <- RM.weights::RM.w(.data)

  rm_prob <- RM.weights::equating.fun(rm)

  rm_item_b <- rm$b |> as_tibble_col('item_severity')
  rm_item_se.b <- rm$se.b |> as_tibble_col('item_severity_se')
  rm_item_infit <- rm$infit |> as_tibble_col('infit_stat')
  rm_item_se.infit <- rm$se.infit |> as_tibble_col('infit_stat_se')
  rm_item_outfit <- rm$outfit |> as_tibble_col('outfit_stat')

  rm_by_item <- as_tibble_col(names(.data), 'name') |>
    add_column(rm_item_b) |>
    add_column(rm_item_se.b) |>
    add_column(rm_item_infit) |>
    add_column(rm_item_se.infit) |>
    add_column(rm_item_outfit)

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

  rm_by_score <- as_tibble_col(c(0:8), 'raw_score') |>
    mutate(raw_score = as.character(raw_score)) |>
    add_column(rm_score_a) |>
    add_column(rm_score_se.a) |>
    add_column(rm_score_prob)

  rm_by_prevalence <- tibble(
    prevalence_moderate_and_severe = 100 * rm_prob$prevs[[1]],
    prevalence_severe = 100 * rm_prob$prevs[[2]],
    reliability = rm$reliab,
    reliability_equal_weights = rm$reliab.fl
  )

  return(
    list(
      prevalence = rm_by_prevalence,
      item = rm_by_item,
      score = rm_by_score
    )
  )
}


