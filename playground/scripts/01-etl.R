library(rcbms)

load_required_packages(.load_dependencies = F)

config <- set_config('playground/configs/global.yml')
refs <- load_references(config)
parquet <- read_cbms_data(refs, config)

complete_cases <- get_complete_cases(parquet, config)

result <- list()
lapply(get_script_files(), source)

generate_output(result, refs)
