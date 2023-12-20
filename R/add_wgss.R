#' Title
#'
#' @param .data
#' @param .prefix
#'
#' @return
#' @export
#'
#' @examples
#'

add_wgss <- function(.data, .prefix) {

  .data %>%
    mutate_at(vars(matches(paste0('^', .prefix))), as.integer) %>%
    mutate(
      wgss_a = !!as.name(paste0(.prefix, '_a_seeing')),
      wgss_b = !!as.name(paste0(.prefix, '_b_hearing')),
      wgss_c = !!as.name(paste0(.prefix, '_c_walking')),
      wgss_d = !!as.name(paste0(.prefix, '_d_remembering')),
      wgss_e = !!as.name(paste0(.prefix, '_e_self_caring')),
      wgss_f = !!as.name(paste0(.prefix, '_f_communicating'))
    ) %>%
    mutate(
      !!as.name(paste0(.prefix, '_ssdi')) := if_else(
        wgss_a > 2 |
          wgss_b > 2 |
          wgss_c > 2 |
          wgss_d > 2 |
          wgss_e > 2 |
          wgss_f > 2,
        1L,
        2L
      )
    ) %>%
    mutate(
      a_sssco = if_else(wgss_a == 1, 0, 6 ^ (wgss_a - 2)),
      b_sssco = if_else(wgss_b == 1, 0, 6 ^ (wgss_b - 2)),
      c_sssco = if_else(wgss_c == 1, 0, 6 ^ (wgss_c - 2)),
      d_sssco = if_else(wgss_d == 1, 0, 6 ^ (wgss_d - 2)),
      e_sssco = if_else(wgss_e == 1, 0, 6 ^ (wgss_e - 2)),
      f_sssco = if_else(wgss_f == 1, 0, 6 ^ (wgss_f - 2))
    ) %>%
    mutate_at(vars(matches('^wgss_[a-f]$')), as.integer) %>%
    mutate(
      sssco = rowSums(select(., matches('^[a-f]_sssco$')), na.rm = T),
      sssc = case_when(
        sssco == 0 ~ 1L,
        sssco < 5 ~ 2L,
        sssco < 24 ~ 3L,
        sssco >= 24 ~ 4L
      )
    ) %>%
    select(-matches('^[a-f]_sssco$'), -matches('^wgss_[a-f]$')) |>
    rename(
      !!as.name(paste0(.prefix, '_sssco')) := sssco,
      !!as.name(paste0(.prefix, '_sssc')) := sssc
    )
}


