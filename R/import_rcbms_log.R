import_rcbms_log <- function(
  .conn,
  .data,
  .remarks,
  .table_name,
  .by_cv_cols,
  .uid,
  .table_suffix
) {

  DBI::dbWriteTable(.conn, value = .data, name = .table_name, append = T)

  # No remarks
  if(nrow(.remarks) == 0) return(.remarks)

  remarks_join <- .remarks |>
    dplyr::left_join(
      .data |>
        dplyr::select(
          uuid = id,
          dplyr::any_of(.by_cv_cols)
        ),
      by = 'uuid'
    )

  if(.uid %in% names(remarks_join)) {
    remarks_join <- remarks_join |>
      dplyr::filter(!is.na(!!as.name(.uid)))
  }

  # No remarks joined
  if(nrow(remarks_join) == 0) return(.remarks)

  remarks_join <- remarks_join |>
    dplyr::mutate(date = lubridate::as_datetime(created_at)) |>
    dplyr::arrange(dplyr::desc(date)) |>
    dplyr::select(-date)

  remarks_id <- remarks_join$uuid

  df <- DBI::dbReadTable(.conn, .table_name)

  if(nrow(df) == 0) return(.remarks)

  remarks_join <- remarks_join |>
    dplyr::select(-uuid) |>
    dplyr::left_join(
      df |>
        dplyr::select(
          uuid = id,
          dplyr::any_of(.by_cv_cols)
        ),
      by = .by_cv_cols
    ) |>
    dplyr::select(-dplyr::any_of(.by_cv_cols)) |>
    dplyr::distinct(uuid, user_id, status, remarks, .keep_all = T)

  DBI::dbWriteTable(
    conn = .conn,
    value = remarks_join |> dplyr::select(-id),
    name = 'remarks',
    append = T
  )

  remarks <- remarks_join |>
    dplyr::distinct(uuid, status) |>
    dplyr::rename(id = uuid, remarks_status = status)

  if(nrow(df)) {

    DBI::dbWriteTable(
      conn = .conn,
      value = df,
      name = paste0(.table_name, .table_suffix),
      overwrite = T
    )

    DBI::dbExecute(.conn, glue::glue("DELETE FROM {.table_name};"))

    DBI::dbWriteTable(
      conn = .conn,
      value = df |>
        dplyr::left_join(
          remarks,
          by = 'id',
          multiple = 'first'
        ) |>
        dplyr::mutate(
          status = dplyr::if_else(
            !is.na(remarks_status),
            remarks_status,
            status
          )
        ) |>
        dplyr::select(-remarks_status),
      name = .table_name,
      append = T
    )
  }

  .remarks |>
    dplyr::filter(!(uuid %in% remarks_id))

}
