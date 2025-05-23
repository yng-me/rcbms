create_summary_stat <- function(.input_data, .config) {

  total <- 0
  summary_info <- list()

  if(exists('parquet')) {

    summary_record <- .config$project[[.input_data]]$summary_record

    if(is.null(summary_record)) {
      return(list(total = total, summary_info = summary_info))
    }

    summary_df <- parquet[[.input_data]][[summary_record]]

    if(is.null(summary_df)) {
      return(list(total = total, summary_info = summary_info))
    }

    final_status_vars <- .config$project[[.input_data]]$final_status$variable

    summary_df <- summary_df |>
      dplyr::select(dplyr::any_of(final_status_vars)) |>
      dplyr::collect()

    if(.input_data == 'hp') {

      # household
      for(k in seq_along(final_status_vars)) {

        final_status_var <- final_status_vars[k]

        summary_info[[final_status_var]] <- summary_df |>
          dplyr::rename(key = !!as.name(final_status_var)) |>
          dplyr::count(key, name = 'value') |>
          dplyr::mutate(
            key = dplyr::if_else(
              is.na(key),
              'NA',
              as.character(key)
            )
          ) |>
          janitor::adorn_totals()
      }

      # roster
      roster_record <- .config$project$hp$roster_record
      sex_var <- .config$project$hp$variable$sex
      age_var <- .config$project$hp$variable$age

      if(!is.null(roster_record) & !is.null(sex_var) & !is.null(age_var)) {

        roster_df <- parquet[[.input_data]][[roster_record]] |>
          dplyr::select(hsn, dplyr::any_of(sex_var), dplyr::contains('_age')) |>
          dplyr::collect() |>
          dplyr::filter(
            as.integer(hsn) < as.integer(
              paste(rep(7, 4 + .config$project$add_length), collapse = "")
            )
          )

        roster_df_names <- names(roster_df)

        summary_info$sex <- roster_df |>
          dplyr::rename(key = !!as.name(sex_var)) |>
          dplyr::count(key, name = 'value') |>
          dplyr::mutate(
            key = dplyr::if_else(
              is.na(key),
              'NA',
              as.character(key)
            )
          ) |>
          janitor::adorn_totals()

        if(length(grepl('_age_group_five_year$', roster_df_names)) >= 1) {

          age_group_var <- roster_df_names[grepl('_age_group_five_year$', names(roster_df))]

          roster_df <- roster_df |>
            dplyr::select(
              age = !!as.name(age_group_var[1]),
              sex = !!as.name(sex_var)
            )

          summary_info$age_group <- roster_df |>
            generate_tab(.cols = c('age', 'sex'), .total_by_cols = T) |>
            factor_col('age') |>
            factor_col('sex') |>
            dplyr::bind_rows(
              roster_df |>
                generate_tab(.cols = 'age') |>
                factor_col('age') |>
                dplyr::mutate(sex = 0, sex_fct = 'Both sexes', total_y = total)
            )

          summary_info$age_group_pivot <- summary_info$age_group |>
            dplyr::arrange(age) |>
            dplyr::ungroup() |>
            dplyr::filter(!is.na(age), !is.na(sex_fct)) |>
            dplyr::select(age, sex_fct, count, percent) |>
            tidyr::pivot_wider(
              names_from = sex_fct,
              values_from = c(count, percent)
            ) |>
            dplyr::left_join(
              data.frame(
                age = 1L:21L,
                age_group = c(
                  "< 5",
                  "5-9",
                  "10-14",
                  "15-19",
                  "20-24",
                  "25-29",
                  "30-34",
                  "35-39",
                  "40-44",
                  "45-49",
                  "50-54",
                  "55-59",
                  "60-64",
                  "65-69",
                  "70-74",
                  "75-79",
                  "80-84",
                  "85-89",
                  "90-94",
                  "95-99",
                  "100+"
                )
              ),
              by = 'age'
            ) |>
            dplyr::select(age_group, age, dplyr::everything()) |>
            janitor::adorn_totals()
        }
      }
    }

    total <- nrow(summary_df)

  }

  list(
    total = total,
    summary_info = summary_info
  )

}
