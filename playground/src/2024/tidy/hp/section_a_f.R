df_temp_tidy <- df_temp |>
  mutate(
    a01_name_of_hh_member = str_replace(
      str_replace_all(a01_name_of_hh_member, "\\s+", " "),
      "\\s,", ","
    )
  )
