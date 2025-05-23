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
    "CURRENT_INPUT_DATA",
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
        delete_source = FALSE,
        create_lineage = TRUE,
        read_from_chunk = FALSE,
        lineage_frequency = 'daily',
        partition_by = c("region_code", "province_code", "city_mun_code")
      ),
      reload_references = list(
        area_name = FALSE,
        area_name_new = FALSE,
        valueset = FALSE,
        validation = FALSE,
        tabulation = FALSE,
        data_dictionary = FALSE,
        macrodata = FALSE,
        score_card = FALSE,
        record = FALSE,
        section = FALSE,
        mpi_spec = FALSE
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
        level = 4,
        areas = "all",
        by_area = FALSE
      ),
      validation = list(
        priority_level = c("A", "B", "C", "D"),
        include_signature = FALSE,
        include_extension = FALSE,
        include_prelim = FALSE,
        generate_output = FALSE,
        detailed_output = FALSE,
        check_duplicate_members = FALSE,
        include_contact_info = TRUE,
        include_additional_info = FALSE,
        validate_signatures = FALSE,
        add_uuid = TRUE,
        save_as_excel = FALSE,
        save_to_db = TRUE,
        save_as_json = TRUE,
        stringify_info = TRUE,
        signature = list(
          crop_threshold = 1000,
          filename_length = 31,
          ink_threshold = 375,
          img_threshold = 100000,
          path = NULL,
          valid = list(
            path = NULL,
            action = NULL
          ),
          invalid = list(
            path = NULL,
            action = NULL
          )
        )
      ),
      tabulation = list(
        generate_output = FALSE
      ),
      portal = list(
        stage = "dev",
        db_migration = list(
          overwrite = TRUE,
          append = FALSE,
          local_infile = TRUE,
          migrate_refs = FALSE,
          geojson_path = ""
        )
      ),
      version = list(
        app = "3.0.1",
        script = "0.0.1",
        package = "0.1.9",
        db = "3"
      ),
      clear_objects = TRUE,
      warning = FALSE,
      verbose = TRUE,
      progress = FALSE,
      user_id = NULL
    )
  )

  toset <- !(names(op.rcbms) %in% names(op))
  if (any(toset)) options(op.rcbms[toset])

  invisible()
}
