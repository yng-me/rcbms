#' Title
#'
#' @param .dir
#' @param .rm
#'
#' @return
#' @export
#'
#' @examples
create_new_folder <- function(.dir, .rm = FALSE) {
  if (dir.exists(.dir) & .rm) {
    unlink(.dir, recursive = TRUE)
  }

  if (!dir.exists(.dir)) {
    dir.create(.dir, recursive = TRUE)
  }
  return(.dir)
}

#' Title
#'
#' @param .site
#'
#' @return
#' @export
#'
#' @examples
is_online <- function(.site = "http://google.com/") {
  tryCatch(
    {
      readLines(.site, n = 1)
      TRUE
    },
    warning = function(w) invokeRestart("muffleWarning"),
    error = function(e) FALSE
  )
}

#' Title
#'
#' @param path
#'
#' @return
#' @export
#'
#' @examples
clean_path <- function(path) {
  stringr::str_replace_all(
    stringr::str_replace_all(path, "/\\.?/", "/"), "\\/+", "/"
  )
}


#' Title
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
join_path <- function(...) {
  if (Sys.info()[1] == "Darwin" | Sys.info()[1] == "darwin") {
    path <- stringr::str_c(..., collapse = "", sep = "/")
  } else {
    path <- stringr::str_c(..., collapse = "", sep = "\\")
  }

  return(clean_path(path))
}


#' Title
#'
#' @return
#' @export
#'
#' @examples
format_current_date <- function() {
  formatted_date <- paste0(
    stringr::str_pad(lubridate::day(lubridate::today()), width = 2, pad = "0"), " ",
    lubridate::month(lubridate::today(), label = TRUE, abbr = FALSE), " ",
    lubridate::year(lubridate::today())
  )
  return(formatted_date)
}


#' Title
#'
#' @param .data
#'
#' @return
#' @export
#'
#' @examples
clean_colnames <- function(.data) {
  str_to_replace <- "\\.|\\-|\\s|\\$\\>|\\<|\\|\\(|\\)|\\[|\\]"
  .data |>
    dplyr::rename_with(~ tolower(stringr::str_replace_all(., str_to_replace, "_"))) |>
    dplyr::rename_with(~ stringr::str_remove(., "^x\\.\\.\\."))
}


#' Set current working directory relative to the project path
#'
#' @param .base_wd
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
set_relative_wd <- function(
    ...,
    .base_wd = getOption("rcbms.config")$working_directory) {
  if (!is.null(.base_wd)) {
    if (!is.null(.base_wd) & typeof(.base_wd) == "character") {
      wd <- .base_wd
    } else if (!is.null(.base_wd$base)) {
      wd <- .base_wd
    }
  } else {
    wd <- "."
  }

  join_path(paste0(wd, "/", ...))
}


set_class <- function(.data, .class) {
  class(.data) <- c(.class, class(.data))
  return(.data)
}

is_not_installed <- function(.pkg) {
  rlang::is_false(rlang::is_installed(.pkg))
}

#' Title
#'
#' @param .type
#'
#' @return
#' @export
#'
#' @examples
rcbms_list <- function(.type) {
  new_list <- list()
  set_class(new_list, paste0("rcbms_", .type, "_list"))
}


#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
hex_to_raw <- function(x) {
  digits <- strtoi(strsplit(x, "")[[1]], base = 16L)
  as.raw(bitwShiftL(digits[c(TRUE, FALSE)], 4) + digits[c(FALSE, TRUE)])
}


#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples

raw_to_hex <- function(x) {
  if (!is.raw(x)) {
    stop("Input must be a raw vector.")
  }

  hex_string <- paste0(sprintf("%02X", as.integer(x)), collapse = "")

  return(hex_string)
}



#' Title
#'
#' @param passphrase
#'
#' @return
#' @export
#'
#' @examples
generate_aes_key <- function(passphrase = '', app_key = NULL, pub_key = NULL) {

  key <- raw_to_hex(openssl::aes_keygen())
  salt <- openssl::sha256(passphrase)

  key_plain <- openssl::sha256(paste0(key, salt))
  iv_plain <- raw_to_hex(openssl::rand_bytes(16))

  out <- list(
    key = key_plain,
    iv = iv_plain
  )

  if(!is.null(app_key)) {
    out$key_app <- encrypt_info(key_plain, .key = app_key)
    out$iv_app <- encrypt_info(iv_plain, .key = app_key)
  }

  if(!is.null(pub_key)) {

    out$key_admin <- key_plain |>
      serialize(connection = NULL) |>
      openssl::rsa_encrypt(pubkey = pub_key) |>
      openssl::base64_encode()

    out$iv_admin <- iv_plain |>
      serialize(connection = NULL) |>
      openssl::rsa_encrypt(pubkey = pub_key) |>
      openssl::base64_encode()

  }

  out$key <- as.character(out$key)

  return(out)

}


#' Title
#'
#' @param dir
#' @param key
#'
#' @return
#' @export
#'
#' @examples
#'
write_aes_key <- function(dir, key = generate_aes_key()) {
  text <- paste0(
    'AES_KEY=', key$key,
    '\n',
    'AES_IV=', key$iv
  )
  env <- file.path(dir, '.env')
  if(file.exists(env)) {
    unlink(env, force = T)
  }
  writeLines(text, con = file(env))
}




#' Title
#'
#' @return
#' @export
#'
#' @examples
#'

generate_env <- function(mode = 'test') {

  if(!file.exists('.env')) {
    writeLines(paste0('VITE_MODE=', mode,'\n'), file('.env'))
  }

  env <- rcbms::set_dot_env(".env")
  keys <- c('VITE_APP_KEY', 'VITE_AES_KEY', 'VITE_AES_IV')

  for(i in seq_along(keys)) {
    key_i <- keys[i]
    if(is.null(env[[key_i]])) {

      if(key_i == 'VITE_APP_KEY') {
        app_key <- openssl::rand_bytes(64) |>
          openssl::sha256() |>
          openssl::base64_encode()
      } else if (key_i == 'VITE_AES_KEY') {
        app_key  <- openssl::aes_keygen() |>
          rcbms::raw_to_hex() |>
          openssl::sha256()
      } else if (key_i == 'VITE_AES_IV') {
        app_key <- rcbms::raw_to_hex(openssl::rand_bytes(16))
      }

      env_data <- readr::read_file('.env')
      env_app_key <- paste0(env_data, key_i, '="', app_key, '"')
      writeLines(env_app_key, file('.env'))
    }
  }
}


#' Title
#'
#' @param db
#'
#' @return
#' @export
#'
#' @examples
#'

connect_to_sqlite <- function(db) {
  DBI::dbConnect(RSQLite::SQLite(), db)
}
