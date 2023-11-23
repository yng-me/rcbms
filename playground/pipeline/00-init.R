list_of_packages <- c(
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
  'googlesheets4',
  'foreign'
)

lapply(list_of_packages, load_package)

