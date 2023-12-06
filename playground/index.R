config <- get_config('./playground/config.yml')
ref_dcf <- load_data_dictionary(config$refs$data_dictionary, .cbms_round = 2022)
data_path <- './playground/src/2022/data/raw/045637 Quezon, Quezon/SECTION_A_TO_E.TXT'

df <- read_cbms_data(
  data_path,
  .dictionary = ref_dcf,
  .valueset = ref_vs
  )

df |> dplyr::select()
