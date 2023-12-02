set_config <- function(.config_file, .cwd = NULL) {


  config <- get_config(.config_file, .cwd)
  if(is.null(.cwd)) { .cwd <- './' }
  base <- join_path(.cwd, 'src', config$cbms_round)

  project_config_path <- join_path(base, 'config.json')

  if(!file.exists(project_config_path)) {
    stop(cat(crayon::red(('Project configuration file is missing: config.json'))))
  }

  config$project <- set_config(project_config_path)

  # Define input data ------------------------------------------------------------
  if(config$input_data == 'all') {
    input_data <- config$project$data_files[[1]]
  } else {
    input_data <- config$input_data
  }


  mode_type_path <- join_path(cwd, 'core/modes/types', paste0(config$mode$type, '.R'))
  mode_type <- config$mode$type

  mode_folder_keys <- c('tabulation', 'validation', 'mpi', 'eda', 'presentation', 'signature-validation')
  mode_folder_names <- c('Tables', 'Inconsistencies', 'MPI', 'Others', 'Presentation', 'Signature')

  output_path <- join_path(cwd, config$project$output_path)

  tb_output_dir <- join_path(
    output_path,
    mode_folder_names[which(mode_folder_keys == mode_type)]
  )

  tb_output_path <- join_path(tb_output_dir, formatted_date)


  create_new_folder(name = output_path)
  create_new_folder(name = tb_output_dir)
  create_new_folder(name = tb_output_path)

  aggregation <- config$project$aggregation
  n_level <- aggregation$level
  agg_record <- aggregation$src$record
  aggregation$label <- aggregation$labels[n_level]
  aggregation$level <- aggregation$levels[n_level]


  options(rcbms_config = config)

}
