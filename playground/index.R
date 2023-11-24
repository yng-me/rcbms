cwd <- './playground/pipeline/'
source(paste0(cwd, '00-init.R'))

age_group_fct <- c(
  '< 5',
  '5-9',
  '10-14',
  '15-19',
  '20-24',
  '25-29',
  '30-34',
  '35-39',
  '40-44',
  '45-49',
  '50-54',
  '55-59',
  '60-64',
  '65-69',
  '70-74',
  '75-79',
  '80-84',
  '85-89',
  '90-94',
  '95-99',
  '100+'
)

import_data <- function(.path, t) {
  readr::read_delim(
    .path,
    delim = '\t',
    quote = "",
    progress = F,
    trim_ws = T,
    show_col_types = F
  ) |>
    janitor::clean_names() |>
    mutate(Barangay = t) |>
    select(-any_of('a04_birthday')) |>
    unite(
      case_id,
      region_code,
      province_code,
      city_mun_code,
      barangay_code,
      ean,
      bsn_code,
      husn_code,
      hsn_code,
      sep = '',
      remove = F
    ) |>
    select(case_id, everything())
}

hp <- list()
records <- c('SECTION_A.TXT', 'SECTION_M.TXT', 'SECTION_P.TXT', 'SECTION_Q.TXT')
df_path <- './playground/src/2024/data/raw/t'

for(i in seq_along(records)) {
  record <- records[i]
  record_name <- tolower(stringr::str_remove(basename(record), '\\.(txt|TXT)$'))
  df_temp <- import_data(paste0(df_path, 1, '/', record), "Daungan")
  hp[[record_name]] <- bind_rows(
    df_temp,
    import_data(paste0(df_path, 2, '/', record), "Malis")
  )
}




library(tsg)
ts <- list()

hp_demog <- hp$section_a |>
  filter(!is.na(as.integer(a05_age)), !is.na(as.integer(a03_sex))) |>
  mutate(
    age = as.integer(a05_age),
    sex = factor(as.integer(a03_sex), c(1, 2), c('Male', 'Female')),
    a07_marital_status = factor(
      a07_marital_status,
      c(1:8),
      c(
        'Single',
        'Married',
        'Common Law/Live-In',
        'Widowed',
        'Divorced',
        'Separated',
        'Annulled',
        'Unknown'
      )
    )
  ) |>
  add_age_groups(age, 'a05') |>
  mutate(a05_age_group_five_year = factor(a05_age_group_five_year, 1:21, age_group_fct))


ts_hh_summary_all <- hp_demog |>
  distinct(case_id, .keep_all = T) |>
  count(Barangay, name = 'Number of households') |>
  janitor::adorn_totals(name = 'Summary')

ts_nuc_all <- hp_demog |>
  group_by(case_id) |>
  summarize(nuc = max(b01_nuclear_family), .groups = 'drop') |>
  ungroup() |>
  summarise(ave_nuc_size = mean(nuc, na.rm = T)) |>
  mutate(Barangay = 'Summary')

ts_nuc <- hp_demog |>
  group_by(case_id, Barangay) |>
  summarize(nuc = max(b01_nuclear_family), .groups = 'drop') |>
  ungroup() |>
  group_by(Barangay) |>
  summarise(ave_nuc_size = mean(nuc, na.rm = T)) |>
  bind_rows(ts_nuc_all)

ts_hh_size_all <- hp_demog |>
  group_by(case_id) |>
  count() |>
  ungroup() |>
  summarise(ave_hh_size = mean(n, na.rm = T)) |>
  mutate(Barangay = 'Summary')

ts_hh_size <- hp_demog |>
  group_by(case_id, Barangay) |>
  count() |>
  ungroup() |>
  group_by(Barangay) |>
  summarise(ave_hh_size = mean(n, na.rm = T)) |>
  bind_rows(ts_hh_size_all)

ts$ts_hh_summary <- ts_hh_summary_all |>
  left_join(ts_hh_size, by = 'Barangay') |>
  left_join(ts_nuc, by = 'Barangay') |>
  rename(
    'Average household size' = ave_hh_size,
    'Average number of nuclear family in a household' = ave_nuc_size
  ) |>
  tibble()

ts$ts_hh_size_dist <- hp_demog |>
  group_by(case_id) |>
  count(name = 'count') |>
  ungroup() |>
  generate_crosstab(count, x_label = 'Household size')

ts$ts_age_sex <- hp_demog |>
  generate_crosstab(Barangay, sex) |>
  mutate(
    'Sex ratio (male:female)' = paste0(
      round(100 * (`Frequency>Male` / `Frequency>Female`)), ':100'
    )
  )

ts$ts_age_group <- hp_demog |>
  generate_crosstab(a05_age_group_five_year, sex, total_by = 'col', x_label = 'Five-year Age Group')

ts$ts_marital <- hp_demog |>
  generate_crosstab(a07_marital_status, x_label = 'Marital Status')

ts$ts_top_3_ethnicity <- hp_demog |>
  generate_frequency(b03_ethnicity, sort_frequency = T, include_cumulative = F) |>
  slice(1:3) |>
  mutate(
    b03_ethnicity = factor(
      b03_ethnicity,
      c(266, 61, 62),
      c('Tagalog', 'Bikol/Bicol', 'Bisaya/Binisaya')
    )
  ) |>
  rename(Ethnicity = b03_ethnicity)

ts$ts_top_3_religion <- hp_demog |>
  generate_frequency(b04_religion, sort_frequency = T, include_cumulative = F) |>
  slice(1:3) |>
  mutate(
    b04_religion = factor(
      b04_religion,
      c(100, 60, 17),
      c('Roman Catholic, Excluding Catholic Charismatic', 'Iglesia Ni Cristo', 'Catholic Charismatic')
    )
  ) |>
  rename(Religion = b04_religion)


ts$ts_reg_lcr <- hp_demog |>
  mutate(b05_phil_id = if_else(is.na(b05_phil_id), 8, b05_phil_id)) |>
  mutate(b05_phil_id = factor(
    b05_phil_id,
    c(1, 2, 8),
    c('With ownership of PhilID', 'Without ownership of PhilID', 'Not reported'))
  ) |>
  generate_crosstab(Barangay, b05_phil_id)


ts$ts_lcr_reg <- hp_demog |>
  mutate(a06_lcr_reg_status = if_else(is.na(a06_lcr_reg_status), 8, a06_lcr_reg_status)) |>
  mutate(a06_lcr_reg_status = factor(
    a06_lcr_reg_status,
    c(1, 2, 8),
    c('Birth registered wit LCR', 'Birth not registered with LCR', 'Not reported'))
  ) |>
  generate_crosstab(Barangay, a06_lcr_reg_status)

ts$ts_literacy <- hp_demog |>
  filter(age >= 5) |>
  mutate(a08_literacy = if_else(is.na(a08_literacy), 8, a08_literacy)) |>
  mutate(a08_literacy = factor(
    a08_literacy,
    c(1, 2, 8),
    c('Literate', 'Illiterate', 'Not reported'))
  ) |>
  generate_crosstab(Barangay, a08_literacy)






p <- hp$section_p |>
  filter(case_id %in% hp_demog$case_id)


ts$ts_main_water <- p |>
  mutate(p01_main_water = if_else(is.na(p01_main_water), 98, p01_main_water)) |>
  mutate(
    p01_main_water = factor(
      p01_main_water,
      c(1:6, 10, 99, 98),
      c(
        'Dwelling',
        'Yard / plot',
        'Public tap',
        'Protected well / tube well / borehole',
        'Unprotected (open dug well)',
        'Developed spring',
        'Tanker truck / peddler / neighbor',
        'Other main source of water',
        'Not reported'
      )
    )
  ) |>
  generate_crosstab(Barangay, p01_main_water, include_frequency = F)

ts$ts_drinking_water <- p |>
  mutate(p03_drinking_water = as.integer(p03_drinking_water)) |>
  mutate(p03_drinking_water = if_else(is.na(p03_drinking_water), 98, p03_drinking_water)) |>
  mutate(
    p03_drinking_water = factor(
      p03_drinking_water,
      c(11, 12, 21, 31, 72, 91, 98),
      c(
        'Piped into dwelling',
        'Piped to yard / plot',
        'Tubed well / borehole',
        'Protected well',
        'Water refilling station',
        'Bottled water',
        'Not reported'
      )
    )
  ) |>
  generate_crosstab(Barangay, p03_drinking_water, include_frequency = F)



ts$ts_toilet <- p |>
  mutate(p09_toilet_facility = as.integer(p09_toilet_facility)) |>
  mutate(p09_toilet_facility = if_else(is.na(p09_toilet_facility), 98, p09_toilet_facility)) |>
  mutate(
    p09_toilet_facility = factor(
      p09_toilet_facility,
      c(11:15, 21:23, 41, 71, 95, 99, 98),
      c(
        "Flush to piped sewer system",
        "Flush to septic tank",
        "Flush to pit latrine",
        "Flush to open drain",
        "Flush to don't know where",
        "Ventilated improved latrine",
        "Pit latrine with slab",
        "Pit latrine without slab / open pit",
        "Bucket",
        "Public toilet",
        "No facility / bush / field",
        "Other",
        "Not reported"
      )
    )
  ) |>
  generate_crosstab(Barangay, p09_toilet_facility, include_frequency = F)



q <- hp$section_q |>
  filter(case_id %in% hp_demog$case_id)


ts$ts_bldg_type <- q |>
  mutate(q01_building_type = as.integer(q01_building_type)) |>
  mutate(q01_building_type = if_else(is.na(q01_building_type), 8, q01_building_type)) |>
  mutate(
    q01_building_type = factor(
      q01_building_type,
      c(1:3, 5, 8),
      c(
        "Single House",
        "Duplex",
        "Apartment / accessoria / rowhouse",
        "Other multi-unit residential",
        "Not reported"
      )
    )
  ) |>
  generate_crosstab(Barangay, q01_building_type, include_frequency = F)

ts$ts_ofi <- hp_demog |>
  filter(age >= 15) |>
  mutate(c03_ofi = as.integer(c03_ofi)) |>
  mutate(c03_ofi = if_else(is.na(c03_ofi), 8, c03_ofi)) |>
  mutate(
    c03_ofi = factor(
      c03_ofi,
      c(1, 7, 8),
      c(
        'Overseas Filipino Worker (OFW) with contract',
        'Resident',
        'Not reported'
      )
    )
  ) |>
  generate_crosstab(Barangay, c03_ofi)


ts$ts_electricity <- q |>
  mutate(q10_electricity = as.integer(q10_electricity)) |>
  mutate(q10_electricity = if_else(is.na(q10_electricity), 8, q10_electricity)) |>
  mutate(
    q10_electricity = factor(
      q10_electricity,
      c(1, 2, 8),
      c(
        "With access to electricity",
        "Without access to electricity",
        "Not reported"
      )
    )
  ) |>
  generate_crosstab(Barangay, q10_electricity, include_frequency = F)


m <- hp$section_m |>
  filter(case_id %in% hp_demog$case_id)

ts$ts_internet <- m |>
  mutate(m01_internet_access = as.integer(m01_internet_access)) |>
  mutate(m01_internet_access = if_else(is.na(m01_internet_access), 8, m01_internet_access)) |>
  mutate(
    m01_internet_access = factor(
      m01_internet_access,
      c(1, 2, 8),
      c(
        "With access to internet",
        "Without access to internet",
        "Not reported"
      )
    )
  ) |>
  generate_crosstab(Barangay, m01_internet_access, include_frequency = F)


ts$ts_tenure <- q |>
  mutate(q08_tenure = as.integer(q08_tenure)) |>
  mutate(q08_tenure = if_else(is.na(q08_tenure), 8, q08_tenure)) |>
  mutate(
    q08_tenure = factor(
      q08_tenure,
      c(1:8),
      c(
        "Own or owner-like possession of the house and lot",
        "Own house, rent lot",
        "Own house, rent-free lot with consent of owner",
        "Own house, rent-free lot without consent of owner",
        "Rent house / room, including lot",
        "Rent-free house and lot with consent of owner",
        "Rent-free house and lot without consent of owner",
        "Not reported"
      )
    )
  ) |>
  generate_crosstab(Barangay, q08_tenure, include_frequency = F)


solo_parent <- hp_demog |>
  mutate(
    b07_solo_parent = as.integer(b07_solo_parent),
    b08_solo_parent_id = as.integer(b08_solo_parent_id)
  ) |>
  filter(b07_solo_parent == 1, age >= 10) |>
  mutate(b08_solo_parent_id = if_else(is.na(b08_solo_parent_id), 8, b08_solo_parent_id)) |>
  mutate(
    b08_solo_parent_id = factor(
      b08_solo_parent_id,
      c(1, 2, 8),
      c(
        "With Solo Parent ID",
        "Without Solo Parent ID",
        "Not reported"
      )
    )
  )

solo_parent_with_id <- solo_parent |>
  group_by(Barangay) |>
  count() |>
  janitor::adorn_totals()

ts$ts_solo_parent <- solo_parent |>
  generate_crosstab(Barangay, b08_solo_parent_id, include_frequency = F) |>
  left_join(solo_parent_with_id, by = 'Barangay')



senior_citizen <- hp_demog |>
  filter(age >= 60) |>
  mutate(
    b09_senior_citizen_id = as.integer(b09_senior_citizen_id),
  ) |>
  mutate(b09_senior_citizen_id = if_else(
    is.na(b09_senior_citizen_id), 8, b09_senior_citizen_id)
  ) |>
  mutate(
    b09_senior_citizen_id = factor(
      b09_senior_citizen_id,
      c(1, 2, 8),
      c(
        "With Solo Parent ID",
        "Without Solo Parent ID",
        "Not reported"
      )
    )
  )


senior_citizen_id <- senior_citizen |>
  group_by(Barangay) |>
  count() |>
  janitor::adorn_totals()

ts$ts_senior_citizen_id <- senior_citizen |>
  generate_crosstab(Barangay, b09_senior_citizen_id, include_frequency = F) |>
  left_join(senior_citizen_id, by = 'Barangay')


save_as_excel(ts, filename = 'Pretest 2 Summary 2.xlsx')


popcen <- hp_demog |>
  select(
    2:22
  ) |>
  mutate(a07_marital_status = as.integer(a07_marital_status)) |>
  select(-a01_name_of_hh_member) |>
  convert_to_na()



write.csv(popcen, 'POPCEN.csv')
