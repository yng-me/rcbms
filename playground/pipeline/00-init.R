# Load/install required packages ----------
library(rcbms)
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
  'googlesheets4'
)

sapply(list_of_packages, load_package)
set_config('playground/config.yml')

refs <- load_refs()
