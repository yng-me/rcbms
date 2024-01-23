#' Title
#'
#' @param .data
#' @param .primary_member
#' @param .sex_of_primary_member
#' @param .relation_to_primary_member
#' @param .threshold
#' @param .condition
#' @param .config
#'
#' @return
#' @export
#'
#' @examples
#'

check_age_sex_relation <- function(
  .data,
  .primary_member,
  .sex_of_primary_member = c(1, 2),
  .relation_to_primary_member,
  .threshold,
  .condition = "<",
  .conjuction = NULL,
  .config = getOption("rcbms.config")
) {

  var <- .config$project$hp$variable
  relation_to_hh_head_var <- var$relation_to_hh_head
  age_var <- var$age
  sex_var <- var$sex

  .data |>
    dplyr::group_by(case_id) |>
    tidyr::nest() |>
    dplyr::mutate(
      data = purrr::map(data, \(x) {

        df <- x |>
          dplyr::mutate(
            age = !!as.name(age_var),
            relation_to_hh_head = as.integer(!!as.name(relation_to_hh_head_var)),
            is_primary_member = relation_to_hh_head == .primary_member & !!as.name(sex_var) %in% .sex_of_primary_member,
            with_relation = relation_to_hh_head == .relation_to_primary_member
          ) |>
          dplyr::filter(is_primary_member | with_relation)

        df_list <- list()
        df_first <- df |>
          dplyr::filter(relation_to_hh_head == .primary_member) |>
          dplyr::mutate(
            primary_member = .primary_member,
            sex_of_primary_member = dplyr::if_else(
              length(.sex_of_primary_member) == 1,
              .sex_of_primary_member[1],
              NA_integer_
            ),
            relation_to_primary_member = .relation_to_primary_member,
            age_of_primary_member = NA_integer_,
            age_of_relation_to_primary_member = NA_integer_,
            age_difference = NA_integer_
          )
        df_list[[1]] <- df_first

        df_head <- df |> dplyr::filter(is_primary_member)
        df_relation <- df |> dplyr::filter(with_relation)

        if(nrow(df_head) == 1 & nrow(df_relation) > 1) {

          hh_head_d <- df |> dplyr::filter(is_primary_member) |> dplyr::pull(age)
          hh_relation_d <- df |> dplyr::filter(with_relation) |> dplyr::pull(age)

          for(i in seq_along(hh_relation_d)) {

            age_diff <- paste("abs(", hh_head_d[1], '-', hh_relation_d[i], ")")

            expr <- NULL
            .conjuction <- c(.conjuction, '')
            for(j in seq_along(.condition)) {
              expr_j <- paste(age_diff, .condition[j], .threshold[j], .conjuction[j])
              expr <- c(expr, expr_j)
            }

            expr_c <- paste(expr, collapse = '')
            if(eval(parse(text = expr_c))) {
              df_list[[i]] <- df_first |>
                dplyr::mutate(
                  age_of_primary_member = eval(parse(text = hh_head_d[1])),
                  age_of_relation_to_primary_member = eval(parse(text = hh_relation_d[i])),
                  age_difference = eval(parse(text = paste0(age_diff)))
                )
            }
          }
        }
        df_final <- do.call("rbind", df_list) |> dplyr::tibble()
        return(df_final)
      })
    ) |>
    tidyr::unnest(data) |>
    validate_select(
      primary_member,
      sex_of_primary_member,
      relation_to_primary_member,
      age_of_primary_member,
      age_of_relation_to_primary_member,
      age_difference
    ) |>
    dplyr::filter(!is.na(age_difference))
}
