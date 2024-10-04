import_rcbms_log <- function(.conn, .data, .remarks, .input_data, .name, .type = 'cv') {

  DBI::dbWriteTable(.conn, value = .data, name = .name, append = T)
  if(nrow(.remarks) == 0) return(.remarks)

  uid_cols <- get_uid_cols(.input_data)
  if(.type == 'ts') {
    uid_cols$by_cv_cols <- 'tabulation_id'
  }

  remarks_join <- .remarks |>
    dplyr::left_join(
      .data |>
        dplyr::select(uuid = id, dplyr::any_of(uid_cols$by_cv_cols)),
      by = 'uuid'
    ) |>
    dplyr::filter(!is.na(!!as.name(uid_cols$uid))) |>
    dplyr::arrange(dplyr::desc(created_at))

  if(nrow(remarks_join) == 0) return(.remarks)

  df <- DBI::dbReadTable(.conn, .name)

  if(nrow(df) == 0) return(.remarks)

  remarks_join <- remarks_join |>
    dplyr::select(-uuid) |>
    dplyr::left_join(
      df |>
        dplyr::select(uuid = id, dplyr::any_of(uid_cols$by_cv_cols)),
      by = uid_cols$by_cv_cols
    ) |>
    dplyr::select(-dplyr::any_of(uid_cols$by_cv_cols)) |>
    dplyr::distinct(uuid, user_id, status, remarks, .keep_all = T)

  DBI::dbWriteTable(
    .conn,
    value = remarks_join |>
      dplyr::arrange(id, created_at) |>
      dplyr::select(-id),
    name = 'remarks',
    append = T
  )

  print('here-1')
  remarks <- remarks_join |>
    dplyr::distinct(uuid, status) |>
    dplyr::rename(id = uuid, remarks_status = status)


  print('here-2')
  DBI::dbWriteTable(
    .conn,
    value = df |>
      dplyr::left_join(
        remarks,
        by = 'id'
      ) |>
      dplyr::mutate(
        status = dplyr::if_else(
          !is.na(remarks_status),
          remarks_status,
          status
        )
      ) |>
      dplyr::select(-remarks_status),
    name = .name,
    overwrite = T
  )
  print('here-3')

  .remarks |>
    dplyr::filter(!(id %in% remarks_join$id))

}


get_uid_cols <- function(.input_data) {
  uid <- 'case_id'
  by_cv_cols <- c(uid, "validation_id", "line_number")

  if(.input_data == 'shp') {
    uid <- 'cbms_geoid'
    by_cv_cols <- c(uid, "validation_id")
  } else if(.input_data == 'bp') {
    uid <- 'barangay_geo'
    by_cv_cols <- c(uid, "validation_id")
  }
  return(
    list(
      uid = uid,
      by_cv_cols = by_cv_cols
    )
  )
}
