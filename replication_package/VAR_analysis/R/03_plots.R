prepare_iv_plot_data <- function(result) {
  if (!is.null(result$plot_irfs)) {
    return(result$plot_irfs)
  }
  if (is.null(result$inference_irfs)) {
    point <- result$irf
    point$Lower68 <- NA_real_
    point$Upper68 <- NA_real_
    point$Lower90 <- NA_real_
    point$Upper90 <- NA_real_
    return(point)
  }

  raw <- as.data.frame(result$inference_irfs)
  raw$variable <- as.character(raw$variable)
  raw$confi_level <- as.character(raw$confi_level)
  selected <- raw[
    raw$confi_type == "msw" &
      raw$confi_level %in% c("0.68", "0.9", "0.90"),
    ,
    drop = FALSE
  ]
  wide <- tidyr::pivot_wider(
    selected,
    id_cols = c("variable", "horizon", "confi_level"),
    names_from = "type",
    values_from = "value"
  )
  point <- wide[
    wide$confi_level == "0.68",
    c("variable", "horizon", "point"),
    drop = FALSE
  ]
  names(point) <- c("Variable", "Horizon", "Estimate")
  band68 <- wide[
    wide$confi_level == "0.68",
    c("variable", "horizon", "lower", "upper"),
    drop = FALSE
  ]
  names(band68) <- c("Variable", "Horizon", "Lower68", "Upper68")
  band90 <- wide[
    wide$confi_level %in% c("0.9", "0.90"),
    c("variable", "horizon", "lower", "upper"),
    drop = FALSE
  ]
  names(band90) <- c("Variable", "Horizon", "Lower90", "Upper90")
  out <- merge(point, band68, by = c("Variable", "Horizon"), all.x = TRUE)
  out <- merge(out, band90, by = c("Variable", "Horizon"), all.x = TRUE)
  out$Horizon <- out$Horizon - 1L
  out$Shock <- result$shock
  out
}

label_irf_variables <- function(data) {
  labels <- c(
    spot_price_log = "Elec Price",
    load_tot_log = "Elec Load",
    gen_tot_log = "Elec Generation",
    gen_forecast_log = "Elec Generation",
    IPI_log = "Ind Prod",
    HICP_log = "HICP",
    ffr = "Policy rate",
    spot_gas_log = "Gas price"
  )
  cholesky_order <- c(
    "Elec Price",
    "Elec Load",
    "Elec Generation",
    "Ind Prod",
    "HICP",
    "Policy rate",
    "Gas price"
  )
  raw <- as.character(data$Variable)
  display <- unname(labels[raw])
  missing_label <- is.na(display)
  display[missing_label] <- raw[missing_label]
  levels <- c(
    cholesky_order[cholesky_order %in% display],
    setdiff(unique(display), cholesky_order)
  )
  data$Variable <- factor(display, levels = levels)
  data
}

plot_iv_irf <- function(result, title, subtitle = "MSW confidence intervals") {
  data <- label_irf_variables(prepare_iv_plot_data(result))
  plot <- ggplot2::ggplot(data, ggplot2::aes(Horizon, Estimate)) +
    ggplot2::geom_hline(
      yintercept = 0, linewidth = 0.3, colour = "grey55"
    )
  if (any(is.finite(data$Lower90) & is.finite(data$Upper90))) {
    plot <- plot + ggplot2::geom_ribbon(
      ggplot2::aes(ymin = Lower90, ymax = Upper90),
      fill = "#9ECAE1", alpha = 0.22, na.rm = TRUE
    )
  }
  if (any(is.finite(data$Lower68) & is.finite(data$Upper68))) {
    plot <- plot + ggplot2::geom_ribbon(
      ggplot2::aes(ymin = Lower68, ymax = Upper68),
      fill = "#3182BD", alpha = 0.28, na.rm = TRUE
    )
  }
  plot +
    ggplot2::geom_line(colour = "#08519C", linewidth = 0.8) +
    ggplot2::facet_wrap(~Variable, scales = "free_y", nrow = 2) +
    ggplot2::labs(
      title = title, subtitle = subtitle,
      x = "Months after shock", y = "Response"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      legend.position = "none",
      panel.grid.minor = ggplot2::element_blank()
    )
}

plot_irf_comparison <- function(results, shock, title, subtitle = NULL) {
  data <- dplyr::bind_rows(lapply(names(results), function(method) {
    frame <- prepare_iv_plot_data(results[[method]])
    frame <- frame[frame$Shock == shock, , drop = FALSE]
    frame$Method <- method
    frame
  }))
  data <- label_irf_variables(data)
  data$Method <- factor(data$Method, levels = names(results))

  confidence_lines <- rbind(
    transform(
      data,
      Confidence = Lower90,
      Confidence_bound = "Lower"
    ),
    transform(
      data,
      Confidence = Upper90,
      Confidence_bound = "Upper"
    )
  )
  confidence_lines <- confidence_lines[
    is.finite(confidence_lines$Confidence),
    ,
    drop = FALSE
  ]

  plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(
      Horizon, Estimate, colour = Method,
      linetype = Method, group = Method
    )
  ) +
    ggplot2::geom_hline(
      yintercept = 0, linewidth = 0.3, colour = "grey55"
    )

  if (nrow(confidence_lines)) {
    plot <- plot + ggplot2::geom_line(
      data = confidence_lines,
      ggplot2::aes(
        y = Confidence,
        group = interaction(Method, Confidence_bound)
      ),
      linetype = "dotted", linewidth = 0.55, na.rm = TRUE
    )
  }

  plot +
    ggplot2::geom_line(linewidth = 0.85) +
    ggplot2::facet_wrap(~Variable, scales = "free_y", nrow = 2) +
    ggplot2::scale_colour_manual(
      values = c("#08519C", "#CB181D", "#238B45", "#756BB1")
    ) +
    ggplot2::labs(
      title = title, subtitle = subtitle,
      x = "Months after shock", y = "Response",
      colour = NULL, linetype = NULL
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      legend.position = "top",
      panel.grid.minor = ggplot2::element_blank()
    )
}
plot_proxy_comparison <- function(benchmark, wind, wedge) {
  plot_irf_comparison(
    list(
      "Wind supply proxy" = wind,
      "Benchmark supply proxy" = benchmark,
      "Iberian wedge only" = wedge
    ),
    shock = "Supply",
    title = "Spain: electricity supply shock",
    subtitle = "Benchmark PCA versus the standalone Iberian wedge proxy"
  )
}
