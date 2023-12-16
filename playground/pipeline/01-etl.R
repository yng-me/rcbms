library(rcbms)
load_required_packages(.load_dependencies = F)
config <- set_config('playground/config.yml')
refs <- load_references(config)
parquet <- import_data(refs$data_dictionary, refs$valueset)

result <- list()
lapply(get_script_files(), source)

if(config$mode$generate_output) {
  generate_output(result)
}
