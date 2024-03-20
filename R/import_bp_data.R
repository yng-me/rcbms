import_bp_data <- function() {
  con <- db_connect(.db_name = "bp")

  bp_records <- c(
    "BPQ_bgy_personnel",
    "BPQ_bgy_purok",
    "BPQ_others_specify",
    "BPQ_page1",
    "BPQ_page2",
    "BPQ_page3",
    "BPQ_page4",
    "BPQ_page5",
    "BPQ_page6",
    "BPQ_page7",
    "BPQ_page8",
    "BPQ_page9",
    "BPQ_page10",
    "BPQ_user"
  )

  bp <- list()
  for (i in seq_along(bp_records)) {
    query_statement <- paste0("SELECT * FROM ", bp_records[i])
    bp[[bp_records[[i]]]] <- DBI::dbGetQuery(con, query_statement) |> dplyr::tibble()
  }

  return(bp)
}
