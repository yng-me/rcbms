ref_year_end <- as.integer(stringr::str_sub(config$project$ref_period, 1, 4))

ref_invalid_keyword <- '^(none|no|all|test|na|try|n.\\a\\.|[-]+)$'
ref_invalid_name <- '^(none|test|na|try|n.\\a\\.)$'
  