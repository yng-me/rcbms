#' Title
#'
#' @param .conn
#' @param .tables
#' @param .input_data
#' @param .uid
#'
#' @return
#' @export
#'
#' @examples

create_logs_table <- function(.conn, .tables, .input_data = 'hp', .uid = 'case_id') {

  line_number <- ''
  contact <- ''
  if(.input_data == 'hp' | .input_data == 'ilq') {
    line_number <- 'line_number varchar(4),'
    contact <- 'contact text,'
  }

  if (!(paste0(.input_data, '_cv') %in% .tables)) {
    DBI::dbExecute(
      .conn,
      paste0(
        "CREATE TABLE ", .input_data, "_cv (
          id varchar(36),
          log_id varchar(36),",
          .uid, " varchar(36),
          validation_id text, ",
          line_number,
          "status tinyint CHECK (status IN (-1, 0, 1, 2, 3, 4, 5, 9)),
          info text,",
          contact,
          "tag_status DATETIME DEFAULT NULL,
          updated_at DATETIME DEFAULT NULL
        );"
      )
    )
  }

  if(!('ts' %in% .tables)) {
    DBI::dbExecute(
      .conn,
      paste0(
        "CREATE TABLE ts (
          id varchar(36),
          log_id varchar(36),
          tabulation_id text,
          area_code varchar(36),
          summary text,
          info text,
          status tinyint CHECK (status IN (-1, 0, 1, 2, 3, 4, 5, 9)),
          tag_status DATETIME DEFAULT NULL,
          updated_at DATETIME DEFAULT NULL
        );"
      )
    )
  }

  if (!("logs" %in% .tables)) {
    DBI::dbExecute(
      .conn,
      "CREATE TABLE logs (
        id_int INTEGER,
        id varchar(36) PRIMARY KEY,
        survey_round varchar(4),
        mode varchar(16),
        edit tinyint CHECK (edit IN (0, 1, 2, 3, 4, 5, 6)),
        level tinyint CHECK (level IN (0, 1, 2, 3, 4, 5)),
        stage tinyint CHECK (stage IN (1, 2, 3, 4, 5, 6)),
        category varchar(32),
        station char(5) CHECK (station IN ('CO', 'RO', 'PO', 'LGU')),
        partial tinyint CHECK (partial IN (0, 1)),
        input_data char(3) CHECK (input_data IN ('hp', 'bp', 'ilq', 'shp')),
        area_code varchar(16),
        area_codes text,
        number_of_ea_processed int,
        total_cases int,
        total_cases_unique int,
        total_priority_a int,
        total_priority_b int,
        total_priority_c int,
        total_priority_d int,
        total int,
        summary text,
        user text,
        duration int,
        source tinyint,
        user_id varchar(36),
        status tinyint CHECK (status IN (-1, 0, 1, 2, 3, 4, 5, 9)),
        version_app varchar(10),
        version_package varchar(10),
        version_script varchar(10),
        pc_user varchar(32),
        pc_effective_user varchar(32),
        pc_os varchar(16),
        pc_os_release_date varchar(36),
        pc_os_version varchar(36),
        pc_pid int,
        pc_hardware varchar(16),
        key_app TEXT,
        key_admin TEXT,
        iv_app TEXT,
        iv_admin TEXT,
        created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
        updated_at DATETIME DEFAULT NULL,
        verified_at DATETIME DEFAULT NULL,
        validated_at DATETIME DEFAULT NULL
      );"
    )
  }
}


create_remarks_table <- function(.conn, .tables) {
  if (!("remarks" %in% .tables)) {
    DBI::dbExecute(
      .conn,
      "CREATE TABLE remarks (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        uuid varchar(36),
        user_id varchar(36),
        username varchar(36),
        first_name varchar(36),
        last_name varchar(36),
        role varchar(36),
        status tinyint CHECK (status IN (-1, 0, 1, 2, 3, 4, 5, 9)),
        remarks text,
        uploaded tinyint CHECK (uploaded IN (0, 1)),
        created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
        tag_status DATETIME DEFAULT NULL,
        updated_at DATETIME DEFAULT NULL
      );"
    )
  }
}
