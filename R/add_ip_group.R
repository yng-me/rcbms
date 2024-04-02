#' Title
#'
#' @param .data
#' @param .ethnicity
#' @param .prefix
#'
#' @return
#' @export
#'
#' @examples
add_ip_group <- function(.data, .ethnicity, .prefix) {
  .data |>
    dplyr::mutate(
      !!as.name(paste0(tolower(.prefix), "_ip_group")) := dplyr::case_when(
        {{ .ethnicity }} %in% c(1:9, 11:38, 40:60, 64:77, 86, 89, 90:96, 98, 100:108, 111, 114:118, 120:136, 138:236, 238:246, 248, 250:255, 259, 262:265, 267:270, 272:277, 279:282, 284, 285, 286) ~ 1L,
        {{ .ethnicity }} %in% c(61:63, 79:82, 84, 85, 87, 88, 109, 110, 197, 199, 237, 247, 249, 260, 266, 283, 287:291, 999) ~ 2L,
        {{ .ethnicity }} %in% c(10, 22, 39, 66, 78, 83, 97, 99, 112, 113, 119, 137, 203, 204, 256, 257, 258, 261, 271, 278) ~ 3L
      )
    )
}
