library(rcbms)

load_required_packages(.load_dependencies = F)

set_config('playground/configs/global.yml')
load_references()
read_cbms_data()
set_aggregation()
execute_script()
