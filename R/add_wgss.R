#' Title
#'
#' @param .data
#' @param .prefix
#'
#' @return
#' @export
#'
#' @examples
add_wgss <- function(.data, .prefix) {
  .data |>
    dplyr::mutate_at(
      dplyr::vars(dplyr::matches(paste0("^", .prefix))),
      as.integer
    ) |>
    dplyr::mutate(
      wgss_a = !!as.name(paste0(.prefix, "_a_seeing")),
      wgss_b = !!as.name(paste0(.prefix, "_b_hearing")),
      wgss_c = !!as.name(paste0(.prefix, "_c_walking")),
      wgss_d = !!as.name(paste0(.prefix, "_d_remembering")),
      wgss_e = !!as.name(paste0(.prefix, "_e_self_caring")),
      wgss_f = !!as.name(paste0(.prefix, "_f_communicating"))
    ) |>
    dplyr::mutate(
      !!as.name(paste0(.prefix, "_ssdi")) := dplyr::if_else(
        wgss_a > 2 |
          wgss_b > 2 |
          wgss_c > 2 |
          wgss_d > 2 |
          wgss_e > 2 |
          wgss_f > 2,
        1L,
        2L
      )
    ) |>
    dplyr::mutate(
      a_sssco = dplyr::if_else(wgss_a == 1, 0, 6^(wgss_a - 2)),
      b_sssco = dplyr::if_else(wgss_b == 1, 0, 6^(wgss_b - 2)),
      c_sssco = dplyr::if_else(wgss_c == 1, 0, 6^(wgss_c - 2)),
      d_sssco = dplyr::if_else(wgss_d == 1, 0, 6^(wgss_d - 2)),
      e_sssco = dplyr::if_else(wgss_e == 1, 0, 6^(wgss_e - 2)),
      f_sssco = dplyr::if_else(wgss_f == 1, 0, 6^(wgss_f - 2))
    ) |>
    dplyr::mutate_at(dplyr::vars(dplyr::matches("^wgss_[a-f]$")), as.integer) |>
    sum_rows("sssco", dplyr::matches("^[a-f]_sssco$")) |>
    dplyr::mutate(
      # sssco = rowSums(dplyr::select(., dplyr::matches('^[a-f]_sssco$')), na.rm = T),
      sssc = dplyr::case_when(
        sssco == 0 ~ 1L,
        sssco < 5 ~ 2L,
        sssco < 24 ~ 3L,
        sssco >= 24 ~ 4L
      )
    ) |>
    dplyr::select(
      -dplyr::matches("^[a-f]_sssco$"),
      -dplyr::matches("^wgss_[a-f]$")
    ) |>
    dplyr::rename(
      !!as.name(paste0(.prefix, "_sssco")) := sssco,
      !!as.name(paste0(.prefix, "_sssc")) := sssc
    )
}
