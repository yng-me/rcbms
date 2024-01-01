df_temp <- df_temp |>
  mutate(
    q02_main_water_distance_range = case_when(
      q01_main_water_other %in% c(1, 9, 10) | q02_main_water_distance == 0 ~ 1L,
      q02_main_water_distance < 10 ~ 2L,
      q02_main_water_distance < 100 ~ 3L,
      q02_main_water_distance < 500 ~ 4L,
      q02_main_water_distance < 1000 ~ 5L,
      q02_main_water_distance >= 1000 ~ 6L
    ),
    q03_improved_drinking_water = if_else(
      q03_drinking_water %in% c(11:14, 21, 31, 41, 51, 61, 71) |
        (q03_drinking_water %in% c(72, 91, 92) &
           !(q04_water_for_cooking_and_handwashing %in% c(32, 42, 81, 99))), 1L, 2L
    ),
    q03_improved_drinking_water_b = if_else(
      q03_drinking_water %in% c(11:14, 21, 31, 41, 51) |
        (q03_drinking_water %in% c(72, 91, 92) &
           !(q04_water_for_cooking_and_handwashing %in% c(32, 42, 61, 71, 81, 99))), 1L, 2L),
    q06_time_to_obtain_drinking_water = case_when(
      q05_drinking_water_source_location %in% c(1, 2) ~ 1L,
      q05_drinking_water_source_location == 3 & q06_time_to_collect_water >= 998 ~ 8L,
      q05_drinking_water_source_location == 3 & q06_time_to_collect_water >= 30 ~ 4L,
      q05_drinking_water_source_location == 3 & q06_time_to_collect_water > 0 ~ 3L,
      q05_drinking_water_source_location == 3 & q06_time_to_collect_water == 0 ~ 2L
    ),
    q03_service_level_drinking_water = case_when(
      q03_drinking_water %in% c(81, 99) ~ 5L,
      q03_drinking_water %in% c(32, 42) ~ 4L,
      q03_improved_drinking_water == 1 & q06_time_to_obtain_drinking_water >= 4 ~ 3L,
      q03_improved_drinking_water == 1 & q06_time_to_obtain_drinking_water %in% c(2, 3) ~ 2L,
      TRUE ~ 1L
    ),
    q10_water_sufficiency = case_when(
      q10_sufficiency_water_supply == 2 ~ 0L,
      q10_sufficiency_water_supply == 8 ~ 8L,
      q10_sufficiency_water_supply == 1 & q11_reason_for_insufficient_water == 1 ~ 1L,
      q10_sufficiency_water_supply == 1 & q11_reason_for_insufficient_water == 2 ~ 2L,
      q10_sufficiency_water_supply == 1 & q11_reason_for_insufficient_water == 3 ~ 3L,
      q10_sufficiency_water_supply == 1 & q11_reason_for_insufficient_water == 8 ~ 8L,
      q10_sufficiency_water_supply == 1 & q11_reason_for_insufficient_water == 9 ~ 9L
    ),
    q14_service_level_toilet_facility = case_when(
      q14_toilet_facility %in% c(95) ~ 4L,
      q14_toilet_facility %in% c(14, 15, 23, 41, 51, 71, 98, 99) ~ 3L,
      # added line below
      q14_toilet_facility %in% c(11, 12, 13, 21, 22, 31) &
        q18_toilet_facility_located == 3 & is.na(q19_share_toilet) ~ 2L,
      q19_share_toilet == 1 ~ 2L,
      q19_share_toilet == 2 ~ 1L
    )  ,
    q23_service_level_handwashing_facility = case_when(

      # Handwashing facility OBSERVED with available water and soap
      (q23_handwashing %in% c(1:3) & q24_handwashing_with_water == 1 & q25_handwashing_with_soap == 1) |

        # OR handwashing facility NOT OBSERVED but with available water and soap
        (q23_handwashing %in% c(4, 9) & q27_available_water_for_washing == 1 &
           q28_available_soap_for_washing == 1 & q29_available_water_and_soap_shown == 1 &
           grepl('[AB]+', q30_soap_for_handwashing)
        ) ~ 1L,

      # Handwashing facility OBSERVED but not available water and/or soap
      (q23_handwashing %in% c(1:3) & (q24_handwashing_with_water == 2 | q25_handwashing_with_soap == 2)) |

        # OR handwashing facility NOT OBSERVED but no available water
        (q23_handwashing %in% c(4, 9) & q27_available_water_for_washing == 2) |

        # OR handwashing facility NOT OBSERVED but with available water but no available soap
        (q23_handwashing %in% c(4, 9) & q27_available_water_for_washing == 1) &
        (
          # NO SOAP
          (q28_available_soap_for_washing == 1 & q29_available_water_and_soap_shown == 2) |

            # With soap but not bar/liquid or detergent
            (q28_available_soap_for_washing == 1 & q29_available_water_and_soap_shown == 1 &
               !grepl('[AB]+', q30_soap_for_handwashing))
        )
      ~ 2L,
      TRUE ~ 3L
    )
  )
