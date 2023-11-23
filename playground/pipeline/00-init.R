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

sapply(list_of_packages, load_package)

