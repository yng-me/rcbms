# cwd <- './playground/pipeline/'
# source(paste0(cwd, '00-init.R'))

data_path <- './playground/src/2024/data/raw/hp/'

all_data_files <- list.files(
    data_path,
    pattern = '\\.TXT$',
    recursive = T,
    full.names = T,
    ignore.case = T
  ) |>
  as_tibble() |>
  mutate(file.info(value))


hp <- list()

for(i in seq_along(all_data_files$value)) {
  file <- all_data_files$value[i]
  record_type <- tolower(stringr::str_remove(basename(file), '\\.(txt|TXT)$'))
  hp[[record_type]] <- readr::read_delim(
      file,
      delim = '\t',
      quote = "",
      progress = F,
      trim_ws = T,
      show_col_types = F
    ) |>
    janitor::clean_names()
}
