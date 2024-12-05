#' Title
#'
#' @param .data
#' @param .path
#'
#' @return
#' @export
#'
#' @examples

download_log <- function(.data, .path, .dcf, .options = list()) {

  wb <- openxlsx::createWorkbook()
  openxlsx::modifyBaseFont(wb, fontName = 'Arial', fontSize = 11)

  for(i in seq_len(nrow(.data))) {

    sheet_name <- .data$table_name[i]

    start_col <- 2
    start_row <- 5
    openxlsx::addWorksheet(wb, sheetName = sheet_name, gridLines = F)
    openxlsx::setColWidths(
      wb,
      sheet = sheet_name,
      cols = 1:4,
      widths = c(2, 20, 10, 32)
    )

    # Table title
    table_number <- paste0('Table ', stringr::str_pad(.data$table_number[i], width = 2, pad = '0'))
    table_title <- paste0(table_number,  '. ', .data$title[i])
    openxlsx::writeData(wb, x = table_title, sheet = sheet_name, startRow = 2, startCol = start_col)
    openxlsx::addStyle(
      wb,
      sheet = sheet_name,
      style = openxlsx::createStyle(
        textDecoration = 'bold',
        valign = 'center',
        fontSize = 13
      ),
      rows = 2,
      cols = start_col
    )

    # Table subtitle
    openxlsx::writeData(wb, x = .data$subtitle[i], sheet = sheet_name, startRow = 3, startCol = start_col)

    df <- jsonlite::fromJSON(.data$info[i])
    df_is_not_a_list <- !inherits(df, 'list')

    if(df_is_not_a_list) {

      df_names <- names(df)

      x_list <- list()
      x_list[[sheet_name]] <- list(
        df = df,
        meta = list(),
        header = data.frame(
          order = 1:ncol(df),
          field = df_names,
          label = df_names
        ),
        attr = list(
          agg_area_level = NULL,
          is_logical_cols = FALSE,
          retain_header = TRUE
        )
      )

      df <- x_list

    }

    df_names <- names(df)


    for(j in seq_along(df_names)) {

      df_name <- df_names[j]
      subtitle_row <- start_row

      if(length(df_names) > 1) {

        df_label <- .dcf |>
          dplyr::filter(value == df_name)

        if(nrow(df_label) > 0) {
          df_label <- df_label$label[1]
        } else {
          df_label <- df_name
        }

        openxlsx::writeData(
          wb,
          x = df_label,
          sheet = sheet_name,
          startRow = subtitle_row,
          startCol = start_col
        )

        openxlsx::addStyle(
          wb,
          sheet = sheet_name,
          style = openxlsx::createStyle(
            textDecoration = 'bold',
            valign = 'center',
            fontSize = 13
          ),
          rows = subtitle_row,
          cols = start_col
        )

        start_row <- start_row + 1
      }

      if(df_is_not_a_list) {

        df_wb <- df[[df_name]][['df']]

      } else {

        df_wb <- df[[df_name]]$df |>
          dplyr::left_join(
            refs_tidy,
            by = c('area_code', 'level')
          ) |>
          dplyr::select(-area_code) |>
          dplyr::select(
            Area = area_name,
            level,
            dplyr::everything()
          ) |>
          dplyr::select(-dplyr::matches('_coded_response$'))
      }


      end_row_offset <- 3
      end_col_offset <- start_col - 1
      end_row <- nrow(df_wb) + end_row_offset
      row_range <- (start_row - 1):(start_row + end_row)
      col_range <- start_col:(ncol(df_wb) + start_col - 1)

      df_wb_names <- names(df_wb)
      df_wb_labels <- df_wb_names |>
        tibble::as_tibble_col(column_name = 'field') |>
        dplyr::left_join(
          df[[j]]$header,
          by = 'field'
        ) |>
        dplyr::mutate(label = dplyr::if_else(
          is.na(label),
          field,
          stringr::str_trim(stringr::str_remove_all(label, '\\(\\%\\)'))
        ))

      # Merges
      freq_cols <- which(grepl('frequency__', df_wb_names, ignore.case = T))
      pcnt_cols <- which(grepl('percent__', df_wb_names, ignore.case = T))
      show_header <- TRUE
      add_offset <- 0

      if(length(freq_cols) > 0 | length(pcnt_cols) > 0) {

        show_header <- FALSE

        openxlsx::writeData(
          wb,
          x = df_wb_labels |>
            dplyr::pull(label) |>
            t(),
          sheet = sheet_name,
          startRow = start_row,
          borders = 'all',
          borderColour = '#bbbbbb',
          startCol = start_col,
          colNames = FALSE
        )

        if(length(freq_cols) > 0) {

          freq_merge_start <- freq_cols[1]
          openxlsx::writeData(
            wb,
            sheet = sheet_name,
            x = 'Frequency',
            startCol = freq_merge_start + end_col_offset,
            startRow = start_row,
          )

          openxlsx::mergeCells(
            wb,
            sheet = sheet_name,
            cols = freq_merge_start:freq_cols[length(freq_cols)] + end_col_offset,
            rows = start_row
          )

          openxlsx::writeData(
            wb,
            x = df_wb_labels$label[freq_cols] |>
              stringr::str_remove_all('frequency__') |>
              t(),
            sheet = sheet_name,
            startRow = start_row + 1,
            borders = 'all',
            borderColour = '#bbbbbb',
            startCol = freq_merge_start + end_col_offset,
            colNames = FALSE
          )

        }

        if(length(pcnt_cols) > 0) {

          pcnt_merge_start <- pcnt_cols[1]

          openxlsx::writeData(
            wb,
            sheet = sheet_name,
            x = 'Percent',
            startCol = pcnt_merge_start + end_col_offset,
            startRow = start_row,
          )

          openxlsx::mergeCells(
            wb,
            sheet = sheet_name,
            cols = pcnt_merge_start:pcnt_cols[length(pcnt_cols)] + end_col_offset,
            rows = start_row
          )

          openxlsx::writeData(
            wb,
            x = df_wb_labels$label[pcnt_cols] |>
              stringr::str_remove_all('percent__') |>
              t(),
            sheet = sheet_name,
            startRow = start_row + 1,
            borders = 'all',
            borderColour = '#bbbbbb',
            startCol = pcnt_merge_start + end_col_offset,
            colNames = FALSE
          )

        }

        add_offset <- 1
        start_row <- start_row + 1

        row_merge_cols <- which(!grepl('(percent|frequency)__', df_wb_names, ignore.case = T))

        for(k in seq_along(row_merge_cols)) {
          openxlsx::mergeCells(
            wb,
            sheet = sheet_name,
            cols = row_merge_cols[k] + end_col_offset,
            rows = (start_row - 1):start_row
          )
        }
      }

      openxlsx::writeData(
        wb,
        x = df_wb,
        sheet = sheet_name,
        startRow = start_row + add_offset,
        borders = 'all',
        borderColour = '#bbbbbb',
        startCol = start_col,
        colNames = show_header
      )

      total_rows <- which(df_wb[[df_name]] == 'Total')

      if(length(total_rows) > 0) {

        if(length(df_names) > 1) {
          sub_offset <- 1
        } else {
          sub_offset <- 0
        }

        openxlsx::addStyle(
          wb,
          sheet = sheet_name,
          style = openxlsx::createStyle(
            textDecoration = 'bold',
          ),
          cols = col_range,
          rows = (start_row + add_offset - sub_offset) + total_rows,
          gridExpand = T,
          stack = T
        )

      }

      openxlsx::addStyle(
        wb,
        sheet = sheet_name,
        style = openxlsx::createStyle(
          fgFill = '#f6f6f6',
          textDecoration = 'bold',
          valign = 'center',
          border = c('top', 'left', 'right'),
          borderColour = '#bbbbbb'
        ),
        cols = col_range,
        rows = (start_row - add_offset):start_row,
        gridExpand = T,
        stack = T
      )

      df_wb_names_pcnt <- which(grepl('percent', df_wb_names, ignore.case = T))

      if(length(df_wb_names_pcnt) > 0) {

        openxlsx::addStyle(
          wb,
          sheet = sheet_name,
          style = openxlsx::createStyle(numFmt = '#,#0.00'),
          cols = df_wb_names_pcnt + (start_col - 1),
          rows = (row_range + 1)[1:(length(row_range) - 1)],
          gridExpand = T,
          stack = T
        )
      }

      df_wb_names_frq <- which(grepl('frequency|total', df_wb_names, ignore.case = T))

      if(length(df_wb_names_frq) > 0) {

        openxlsx::addStyle(
          wb,
          sheet = sheet_name,
          style = openxlsx::createStyle(numFmt = '#,##0'),
          cols = df_wb_names_frq + (start_col - 1),
          rows = (row_range + 1)[1:(length(row_range) - 1)],
          gridExpand = T,
          stack = T
        )
      }

      openxlsx::setRowHeights(
        wb,
        sheet = sheet_name,
        heights = 20,
        rows = row_range + add_offset
      )

      openxlsx::setRowHeights(
        wb,
        sheet = sheet_name,
        heights = 25,
        rows = (start_row - (add_offset + 1)):start_row
      )

      openxlsx::addStyle(
        wb,
        sheet = sheet_name,
        style = openxlsx::createStyle(
          indent = 1,
          valign = 'center'
        ),
        cols = col_range,
        rows = row_range,
        gridExpand = T,
        stack = T
      )

      start_row <- start_row + end_row

      if(length(.options) > 0) {

        if(!is.null(.options$source_note)) {

          openxlsx::writeData(
            wb,
            x = paste0(
              'Source: ',
              .options$source_note[1]
            ),
            sheet = sheet_name,
            startRow = (start_row + 1) - end_row_offset,
            startCol = start_col
          )

          openxlsx::addStyle(
            wb,
            sheet = sheet_name,
            style = openxlsx::createStyle(
              indent = 0,
              fontSize = 9,
              valign = 'center',
              textDecoration = 'italic'
            ),
            cols = start_col,
            rows = (start_row + 1) - end_row_offset,
            gridExpand = T,
            stack = T
          )

          start_row <- start_row + 1
        }
      }

      if(length(df_names) > 1) {
        openxlsx::addStyle(
          wb,
          sheet = sheet_name,
          style = openxlsx::createStyle(
            indent = 0,
            valign = 'center',
            textDecoration = 'bold'
          ),
          cols = start_col,
          rows = subtitle_row,
          gridExpand = T,
          stack = T
        )
      }
    }
  }

  openxlsx::saveWorkbook(wb, .path, overwrite = T)

}
