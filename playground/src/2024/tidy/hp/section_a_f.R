df_temp_tidy <- df_temp |>
  mutate(
    a01_name_of_hh_member = str_replace(
      str_squish(a01_name_of_hh_member),
      "\\s+,",
      ","
    ),
    age = convert_age(
      a04_birthday,
      config$project$reference_period
    ),
    sex = a03_sex
  )
