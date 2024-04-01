utils::globalVariables(
  c(
    ".",
    ":=",
    "n",
    "s",
    "case_id",
    "region_code",
    "province_code",
    "city_mun_code",
    "barangay_code",
    "region",
    "province",
    "city_mun",
    "barangay",
    "barangay_geo",
    "barangay_geo_new",
    "line_number",
    "is_huc",
    "ean",
    "bsn",
    "husn",
    "hsn",
    "result_object",
    "complete_cases",
    "aggregate_level",
    "config",
    "df_temp",
    "df_temp_tidy",
    "add_length",
    "code",
    "name",
    "value",
    "label",
    "refs",
    "Package",
    "variable_name_new",
    "variable_name",
    "is_included",
    "type",
    "title",
    "input_data",
    "validation_id",
    "result",
    "lno",
    "wgss_a",
    "wgss_b",
    "wgss_c",
    "wgss_d",
    "wgss_e",
    "wgss_f",
    "sssco",
    "sssc",
    "variable",
    "valueset",
    "tidy_cbms_data_temp",
    "mode_type",
    "is_included_for_portal",
    "regular_hh_completed",
    "Version",
    "survey_round",
    "current_area_code",
    "current_input_data",
    "relation_to_hh_head",
    "is_primary_member",
    "with_relation",
    "age",
    "primary_member",
    "sex_of_primary_member",
    "relation_to_primary_member",
    "age_of_primary_member",
    "age_of_relation_to_primary_member",
    "age_difference",
    "data",
    "id",
    "assign",
    "log_id",
    "status",
    "id_old",
    "priority_level",
    ".config",
    ".config_key",
    ".assign_name"
  )
)


.onLoad <- function(libname, pkgname) {
  op <- options()
  op.rcbms <- list(
    rcbms.config = list(
      parquet = list(
        convert = TRUE,
        encrypt = TRUE,
        overwrite = TRUE,
        partition = FALSE,
        partition_by = c("region_code", "province_code", "city_mun_code")
      ),
      reload_references = list(
        area_name = FALSE,
        valueset = FALSE,
        validation = FALSE,
        tabulation = FALSE,
        data_dictionary = FALSE,
        macrodata = FALSE,
        score_card = FALSE,
        record = FALSE,
        section = FALSE
      ),
      check_if_online = TRUE,
      complete_cases = TRUE,
      chunk = list(
        read_as_chunk = FALSE,
        threshold = 5000
      ),
      harmonize_variable = TRUE,
      execute_mode = TRUE,
      mode = list(
        type = "validation",
        sub_type = NULL,
        edit = 1,
        stage = 2,
        station = "co"
      ),
      aggregation = list(
        level = 1,
        areas = "all"
      ),
      validation = list(
        generate_output = FALSE,
        detailed_output = FALSE,
        include_additional_info = FALSE,
        validate_signatures = FALSE,
        add_uuid = TRUE,
        save_as_excel = FALSE,
        save_as_json = TRUE
      ),
      tabulation = list(
        generate_output = FALSE
      ),
      portal = list(
        stage = "dev",
        db_migration = list(
          overwrite = TRUE,
          append = FALSE
        )
      ),
      version = list(
        app = "3.0.1",
        script = "0.0.1",
        package = "0.1.9",
        db = "0.0.2"
      ),
      clear_objects = TRUE,
      warning = FALSE,
      verbose = TRUE,
      progress = FALSE
    )
  )

  toset <- !(names(op.rcbms) %in% names(op))
  if (any(toset)) options(op.rcbms[toset])

  invisible()
}
