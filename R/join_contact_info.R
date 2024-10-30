#' Title
#'
#' @param .data
#' @param .input_data
#' @param .uid
#' @param .config
#' @param .encrypt
#'
#' @return
#' @export
#'
#' @examples
#'
join_contact_info <- function(.data, .input_data, .uid, .config, .encrypt = T) {

  summary_record <- .config$project[[.input_data]][['summary_record']]
  contact_info_vars <- .config$project[[.input_data]]$variable$contact
  summary_df <- parquet[[.input_data]][[summary_record]]

  if(is.null(summary_df)) return(.data)

  cases <- .data[[.uid]]

  contact_info <- summary_df |>
    dplyr::select(dplyr::any_of(c(.uid, contact_info_vars)))

  if(.input_data %in% c('hp', 'ilq')) {
    contact_info <- contact_info |>
      dplyr::filter(case_id %in% cases) |>
      dplyr::collect()
  } else if(.input_data == 'bp') {
    contact_info <- contact_info |>
      dplyr::filter(barangay_geo %in% cases) |>
      dplyr::collect()
  }

  contact_info_names <- names(contact_info)
  contact_info_names <- contact_info_names[contact_info_names != .uid]

  contact_info_first <- contact_info |>
    head(1) |>
    collect()

  for(i in seq_along(contact_info_names)) {
    info_name <- contact_info_names[i]

    info_label <- attributes(contact_info_first[[info_name]])$label

    if(!is.null(info_label)) {

      contact_info <- contact_info |>
        dplyr::rename(!!as.name(info_label) := !!as.name(info_name))
    }
  }

  # if(.encrypt & .config$validation$stringify_info) {

    contact_info <- contact_info |>
      dplyr::group_by(!!as.name(.uid)) |>
      tidyr::nest(.key = 'contact') |>
      dplyr::ungroup() |>
      dplyr::tibble() |>
      dplyr::mutate(
        contact = purrr::map_chr(contact, \(x) {
          x |>
            dplyr::mutate_all(as.character) |>
            jsonlite::toJSON() |>
            as.character() |>
            encrypt_info(.config)
        })
      )
#
  # }
  # else {
  #   contact_info <- contact_info |>
  #     dplyr::group_by(!!as.name(.uid)) |>
  #     dplyr::collect() |>
  #     tidyr::nest(.key = 'contact')
  # }

  .data |>
    dplyr::left_join(contact_info, by = .uid)

}
