cwd <- function(...) paste0('./playground/', ...)

set_config(cwd('config.yml'))
env <- read_dot_env(cwd('.env'))
refs <- load_refs(env)

parquet <- import_data(
  .dictionary = refs$data_dictionary,
  .valueset = refs$valueset
)
