check_age_sex_relation <- function(
    .data,
    .base,
    .sex_of_head,
    .relation,
    .condition,
    .threshold
) {
  .data |>
    group_by(case_id) |>
    nest() |>
    mutate(
      data = map(data, \(x) {
        df <- x |>
          transmute(
            age = a05_age,
            a02_relation_to_hh_head = as.integer(a02_relation_to_hh_head),
            is_hh_head = as.integer(a02_relation_to_hh_head) == .base & a03_sex == .sex_of_head,
            with_relation = as.integer(a02_relation_to_hh_head) == .relation
          ) |>
          filter(is_hh_head | with_relation)

        df_head <- df |> filter(is_hh_head)
        df_relation <- df |> filter(with_relation)

        if(nrow(df_head) == 1 & nrow(df_relation) > 1) {

          hh_head_d <- df |> filter(is_hh_head) |> pull(age)
          hh_relation_d <- df |> filter(with_relation) |> pull(age)

          for(i in seq_along(hh_relation_d)) {
            expr <- paste0(
              hh_head_d[1],
              ' - ',
              hh_relation_d[i], ' ',
              .condition, ' ',
              .threshold
            )
            print(eval(parse(text = expr)))
          }
        }
      })
    )
}
