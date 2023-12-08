library(rcbms)
required_packages <- c(
  'arrow',
  'dplyr',
  'tidyr',
  'tibble',
  'stringr',
  'purrr',
  'openxlsx',
  'lubridate',
  'janitor',
  'jsonlite',
  'yaml',
  'readr',
  'quarto',
  'devtools',
  'googlesheets4'
)

sapply(required_packages, load_package)
rm(required_packages)

set_config('playground/config.yml')
refs <- load_refs()


parquet <- import_data(
  .dictionary = refs$data_dictionary,
  .valueset = refs$valueset
)
