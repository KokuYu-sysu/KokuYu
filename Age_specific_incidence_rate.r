age_rate_calculator <- function(data, event, time,
                            strata = NULL, age_strata = NULL,
                            unit = 100000, add_ci = FALSE, conf.level = 0.95) {
  stopifnot(is.data.frame(data))
  stopifnot(is.character(event), event %in% names(data))
  stopifnot(is.character(time), time %in% names(data))
  if (!is.null(strata)) stopifnot(is.character(strata), strata %in% names(data))
  if (!is.null(age_strata)) stopifnot(is.character(age_strata), age_strata %in% names(data))

  df <- data %>%
    mutate(`..event` = as.numeric(.data[[event]]),
           `..time`  = as.numeric(.data[[time]]))

  if (is.null(strata) && is.null(age_strata)) {
    total_py <- sum(df$`..time`, na.rm = TRUE)
    k <- sum(df$`..event`, na.rm = TRUE)
    out <- data.frame(
      count = k,
      person_years = total_py,
      rate = ifelse(total_py > 0, k / total_py * unit, NA_real_)
    )
    if (add_ci) {
      ci <- if (total_py > 0) poisson.test(k, T = total_py, conf.level = conf.level)$conf.int else c(NA_real_, NA_real_)
      out$ci_low <- ci[1] * unit
      out$ci_high <- ci[2] * unit
    }
    return(out)
  }

  # One grouping
  if (!is.null(strata) && is.null(age_strata)) {
    g1 <- rlang::sym(strata)
    out <- df %>%
      group_by(!!g1, .drop = FALSE) %>%
      summarise(
        person_years = sum(`..time`, na.rm = TRUE),
        count = sum(`..event`, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        rate = ifelse(person_years > 0, count / person_years * unit, NA_real_)
      )
    if (add_ci) {
      out <- out %>%
        rowwise() %>%
        mutate(
          .ci = list(if (person_years > 0) poisson.test(count, T = person_years, conf.level = conf.level)$conf.int else c(NA_real_, NA_real_)),
          ci_low = .ci[[1]] * unit,
          ci_high = .ci[[2]] * unit
        ) %>%
        ungroup() %>%
        dplyr::select(-.ci)
    }
    names(out)[1] <- strata
    return(out)
  }

  g1 <- rlang::sym(strata)
  g2 <- rlang::sym(age_strata)
  out <- df %>%
    group_by(!!g1, !!g2, .drop = FALSE) %>%
    summarise(
      person_years = sum(`..time`, na.rm = TRUE),
      count = sum(`..event`, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    tidyr::complete(!!g1, !!g2, fill = list(person_years = 0, count = 0)) %>%
    mutate(
      rate = ifelse(person_years > 0, count / person_years * unit, NA_real_)
    )

  if (add_ci) {
    out <- out %>%
      rowwise() %>%
      mutate(
        .ci = list(if (person_years > 0) poisson.test(count, T = person_years, conf.level = conf.level)$conf.int else c(NA_real_, NA_real_)),
        ci_low = .ci[[1]] * unit,
        ci_high = .ci[[2]] * unit
      ) %>%
      ungroup() %>%
      dplyr::select(-.ci)
  }

  names(out)[1:2] <- c(strata, age_strata)
  out
}
