# RCBMS package

```r
library(rcbms)

load_required_packages(.load_dependencies = F)

config <- set_config('configs/global.yml')
references <- load_references(config)
parquet <- read_cbms_data(references)

aggregation <- set_aggregation(parquet, references)

result <- execute_script(parquet, references, aggregation)
generate_output(result, references, aggregation)

```
