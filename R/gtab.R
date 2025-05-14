#' Title
#'
#' @param .data
#' @param .x
#' @param .y
#'
#' @return
#' @export
#'
#' @examples
#'
gtab <- function(.data, .x, .y) {

  attr_df <- .data |>
    head(1) |>
    dplyr::select({{.x}}, {{.y}}, dplyr::any_of(dplyr::group_vars(.data))) |>
    dplyr::collect()

  attr_object <- setNames(
    lapply(names(attr_df), function(x) {
      attributes(attr_df[[x]])
    }),
    names(attr_df)
  )

  df <- .data |>
    dplyr::group_by({{.x}}, {{.y}}, .add = T) |>
    dplyr::count(name = "Frequency") |>
    dplyr::arrange(desc(Frequency)) |>
    dplyr::ungroup() |>
    dplyr::collect() |>
    tidyr::pivot_wider(
      names_from = {{.y}},
      values_from = Frequency,
      values_fill = 0
    ) |>
    add_factor_meta(attr_object)

  list(
    df = df |>
      janitor::adorn_totals(c('row', 'col')),
    header = attributes(df)$meta |>
      dplyr::add_row(
        order = ncol(df) + 1,
        field = 'Total',
        label = 'Total'
      ),
    attr = attr_object
  )
}


gtab_get_meta <- function(.data, ...) {
  attr_df <- .data |>
    head(1) |>
    dplyr::select(..., dplyr::any_of(dplyr::group_vars(.data))) |>
    dplyr::collect()

  setNames(
    lapply(names(attr_df), function(x) {
      attributes(attr_df[[x]])
    }),
    names(attr_df)
  )
}


add_factor_meta <- function(.data, .meta) {

  df_names <- names(.data)
  meta_labels <- NULL
  meta_types <- NULL

  for(i in seq_along(df_names)) {

    df_name <- df_names[i]
    meta <- .meta[[df_name]]$valueset

    meta_label <- .meta[[df_name]]$label
    if(is.null(meta_label)) meta_label <- df_name

    meta_labels <- c(meta_labels, meta_label)

    if(is.null(meta)) next
    if(!('value' %in% names(meta)) | !('label' %in% names(meta))) next
    .data <- .data |>
      dplyr::mutate(
        !!as.name(df_name) := factor(
          !!as.name(df_name),
          meta$value,
          meta$label
        )
      )
  }

  if(is.null(meta_labels)) {
    meta_labels <- names(.data)
  }

  attr(.data, "meta") <- data.frame(
    order = 1:ncol(.data),
    field = names(.data),
    label = meta_labels
  )

  .data

}
