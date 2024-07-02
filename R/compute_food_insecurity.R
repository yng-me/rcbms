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
compute_food_insecurity <- function(
  .data,
  ...,
  .thres = NULL,
  .type = 1,
  .agg_levels = NULL,
  .valueset = get_config("references")$valueset,
  .extract_name_position = 5,
  .include_overall = FALSE,
  .config = getOption("rcbms.config")
) {

  agg_labels <- c("region", "province", "city_mun", "barangay")

  if (is.null(.agg_levels)) {
    .agg_levels <- c(3, 4)
  }

  rm_by_item_all <- list()
  rm_by_score_all <- list()
  rm_by_prevalence_all <- list()

  for (i in seq_along(.agg_levels)) {
    rm_by_item <- list()
    rm_by_score <- list()
    rm_by_prevalence <- list()

    level_i <- .agg_levels[i]
    agg <- agg_labels[level_i]
    agg_geo <- paste0(agg, "_geo")
    area_codes <- unique(.data[[agg_geo]])

    for (k in seq_along(area_codes)) {

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

    rm_by_score_all[[i]] <- dplyr::bind_rows(rm_by_score) |>
      dplyr::mutate(level = as.integer(level_i))

    rm_by_prevalence_all[[i]] <- dplyr::bind_rows(rm_by_prevalence) |>
      dplyr::mutate(level = as.integer(level_i))
  }

  if(.include_overall) {

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
  }

  by_item <- dplyr::bind_rows(rm_by_item_all)

  if (!is.null(.valueset)) {
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


compute_food_insecurity_prevalence <- function(.data, .thres) {

  rm <- RM.weights::RM.w(.data)
  rm_prob <- apply_equating(rm)

  rm_item_b <- rm$b |> as_tibble_col("item_severity")
  rm_item_se.b <- rm$se.b |> as_tibble_col("item_severity_se")
  rm_item_infit <- rm$infit |> as_tibble_col("infit_stat")
  rm_item_se.infit <- rm$se.infit |> as_tibble_col("infit_stat_se")
  rm_item_outfit <- rm$outfit |> as_tibble_col("outfit_stat")

  rm_by_item <- as_tibble_col(names(.data), "name") |>
    add_column(rm_item_b) |>
    add_column(rm_item_se.b) |>
    add_column(rm_item_infit) |>
    add_column(rm_item_se.infit) |>
    add_column(rm_item_outfit)

  rm_score_a <- rm$a |> as_tibble_col("severity_raw_score")
  rm_score_se.a <- rm$se.a |> as_tibble_col("severity_raw_score_se")
  rm_score_wt.rs <- rm$wt.rs |> as_tibble_col("total_hh")

  rm_score_wt.rel.rs <- rm$wt.rel.rs |>
    as_tibble_col("percent") |>
    mutate(percent = percent * 100)

  # rm_score_prob <- rm_score_wt.rs |>
  #   add_column(rm_score_wt.rel.rs) |>
  #   add_column(as_tibble(rm_prob$probs.rs)) |>
  #   mutate(
  #     mod_sev = percent * `FI_mod+`,
  #     sev = percent * FI_sev
  #   ) |>
  #   select(-starts_with("FI_"))

  rm_by_score <- as_tibble_col(c(0:8), "raw_score") |>
    mutate(raw_score = as.character(raw_score)) |>
    add_column(rm_score_a) |>
    add_column(rm_score_se.a)# |>
    # add_column(rm_score_prob)

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



apply_equating <- function(
  rr1,
  st = NULL,
  tol = 0.35,
  spec.com1 = 1:8,
  spec.com2 = 1:8,
  thres = NULL,
  maxuniq = 3,
  write.file = FALSE,
  plot = FALSE,
  iterative = TRUE,
  excl.prior1 = NULL,
  excl.prior2 = NULL,
  wt.spec = NULL
) {

  tol2 = tol
  b1 = rr1$b
  se1 = rr1$se.b
  if (is.null(st))  {
    st = c(-1.2230564, -0.847121, -1.1056616, 0.3509848,
           -0.3117999, 0.5065051, 0.7546138, 1.8755353)
  }
  b.tot = st
  if (is.null(thres)) {
    thres = b.tot[c(5, 8)]
    truetresh = T
  } else {
    truetresh = F
  }

  common = rep(F, length(b1))
  common[spec.com1] = T
  plus = sum(!common)
  common2 = rep(F, length(b.tot))
  common2[spec.com2] = T
  plus2 = sum(!common2)
  b.st1 = (b1 - mean(b1[spec.com1]))/sd(b1[spec.com1]) * sd(b.tot[spec.com2]) +
    mean(b.tot[spec.com2])
  a = 1
  oldcommon = common
  oldcommon2 = common2
  if (iterative) {
    while (a <= length(b.st1)) {
      oldcommon = common
      oldcommon2 = common2
      diff = rep(100, length(b1))
      diff[spec.com1] = abs(b.st1[spec.com1] - b.tot[spec.com2])
      diff2 = rep(100, length(b.tot))
      diff2[spec.com2] = abs(b.st1[spec.com1] - b.tot[spec.com2])
      ord = order(diff, decreasing = T)
      w = ord[a + plus]
      ord2 = order(diff2, decreasing = T)
      w2 = ord2[a + plus2]
      if (diff[w] >= tol) {
        common[w] = F
      }
      else {
        common[w] = T
      }
      if (diff2[w2] >= tol) {
        common2[w2] = F
      }
      else {
        common2[w2] = T
      }
      scale1 = sd(b.tot[common2])/sd(b.st1[common])
      shift1 = mean(b.tot[common2]) - mean(b.st1[common]) *
        scale1
      b.st1 = shift1 + b.st1 * scale1
      if (sum(oldcommon == common) == length(b.st1) |
          sum(!common) > maxuniq)
        break
      else {
        a = a + 1
      }
    }
  }
  if (!iterative) {
    common[excl.prior1] = F
    common2[excl.prior2] = F
  }
  scale = sd(b.tot[common2])/sd(b1[common])
  shift = mean(b.tot[common2]) - mean(b1[common]) * scale


  newthres = (thres - shift)/scale
  a = rr1$a
  se.a = rr1$se.a
  d = rr1$d
  kk = length(b1) + 1
  if (length(d) > 2) {
    if (d[2] < 0) {
      a = a[c(1, 2:(kk + 1))]
      se.a = se.a[c(1, 2:(kk + 1))]
    }
    else {
      a = a[-kk]
      se.a = se.a[-kk]
    }
  }

  prevs.rs = matrix(NA, length(a), length(newthres))
  wt = rr1$wt
  if (!is.null(wt.spec))
    wt = wt.spec
  XX = rr1$XX
  rv = rowSums(XX)
  k = ncol(XX)
  n_j = sapply(0:(k), function(i) sum(wt[rv == i], na.rm = T))
  f_j = n_j/sum(n_j)
  f_j[1] = 0
  for (i in 1:length(newthres)) {
    prevs.rs[, i] = (1 - pnorm(newthres[i], a, se.a)) *
      f_j
  }

  prevs = colSums(prevs.rs)

  return(
    list(
      prevs = prevs,
      prevs.rs = prevs.rs
    )
  )

}

