proxy_relevance_row <- function(result, label) {
  if (!is.null(result$relevance)) {
    row <- result$relevance
    row$Specification <- label
    return(row)
  }
  equation <- match(result$norm_var, result$variables)
  y <- result$residuals[, equation]
  z <- result$sample[[result$proxy]][-seq_len(result$fit$p)]
  model <- stats::lm(y ~ z)
  summary_model <- summary(model)
  classical_f <- unname(summary_model$fstatistic[["value"]])
  coefficient <- stats::coef(model)[["z"]]
  standard_error <- summary_model$coefficients["z", "Std. Error"]
  robust_f <- NA_real_

  if (
    requireNamespace("sandwich", quietly = TRUE) &&
      requireNamespace("lmtest", quietly = TRUE)
  ) {
    covariance <- sandwich::NeweyWest(
      model,
      lag = result$fit$p,
      prewhite = FALSE,
      adjust = TRUE
    )
    robust_test <- lmtest::coeftest(model, vcov. = covariance)
    robust_f <- unname(robust_test["z", "t value"]^2)
  }

  data.frame(
    Country = result$country,
    Specification = label,
    Shock = result$shock,
    Proxy = result$proxy,
    Normalization_equation = result$norm_var,
    First_stage_coefficient = coefficient,
    First_stage_SE = standard_error,
    F_statistic = classical_f,
    HAC_F_statistic = robust_f,
    Proxy_residual_correlation = stats::cor(z, y, use = "complete.obs"),
    Weak_below_10 = ifelse(
      is.finite(robust_f), robust_f < 10, classical_f < 10
    ),
    stringsAsFactors = FALSE
  )
}

run_relevance <- function(models, cfg,
                          csv_name = "proxy_relevance.csv",
                          tex_name = "iv_relevance_spain_tot.tex") {
  table <- dplyr::bind_rows(lapply(names(models), function(name) {
    proxy_relevance_row(models[[name]], name)
  }))
  save_csv(table, cfg, csv_name)

  display <- table |>
    dplyr::select(
      Country, Specification, Shock,
      F_statistic, HAC_F_statistic, Weak_below_10
    ) |>
    gt::gt(groupname_col = "Country") |>
    gt::fmt_number(
      columns = c(F_statistic, HAC_F_statistic),
      decimals = 2
    ) |>
    gt::cols_label(
      Specification = "Specification",
      Shock = "Shock",
      F_statistic = "F-stat",
      HAC_F_statistic = "HAC F-stat",
      Weak_below_10 = "F < 10"
    ) |>
    gt::tab_header(title = "Proxy relevance") |>
    gt::tab_source_note(
      gt::md("*HAC statistic uses Newey-West covariance when sandwich and lmtest are installed.*")
    )
  save_gt(display, cfg, tex_name)
  table
}
