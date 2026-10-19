combine_single_proxy_hd <- function(supply, demand) {
  if (!identical(as.Date(supply$dates), as.Date(demand$dates))) {
    stop("Supply and demand historical decompositions have different dates.")
  }
  common_variables <- intersect(supply$variables, demand$variables)
  hd <- array(
    NA_real_,
    dim = c(length(supply$dates), length(common_variables), 2L),
    dimnames = list(NULL, common_variables, c("Supply", "Demand"))
  )
  hd[, , "Supply"] <- supply$hd[, common_variables, 1, drop = FALSE][, , 1]
  hd[, , "Demand"] <- demand$hd[, common_variables, 1, drop = FALSE][, , 1]
  list(hd = hd, dates = supply$dates, variables = common_variables)
}

lag_vector <- function(x, lag = 12L) {
  c(rep(NA_real_, lag), head(x, -lag))
}

contribution_table_from_hd <- function(decomposition, windows, country, method,
                                       observed_data) {
  dates <- as.Date(decomposition$dates)
  shocks <- dimnames(decomposition$hd)[[3]]
  hicp <- decomposition$hd[, "HICP_log", , drop = FALSE][, 1, ]
  if (is.vector(hicp)) hicp <- matrix(hicp, ncol = 1L)
  colnames(hicp) <- shocks
  yoy <- 100 * (hicp - apply(hicp, 2, lag_vector))

  assert_columns(
    observed_data,
    c("observation_date", "HICP_log"),
    paste(country, "observed inflation data")
  )
  observed_data <- observed_data[order(observed_data$observation_date), , drop = FALSE]
  observed_dates <- as.Date(observed_data$observation_date)
  monthly_inflation <- c(NA_real_, diff(observed_data$HICP_log))

  rows <- list()
  index <- 1L
  for (episode in names(windows)) {
    period <- windows[[episode]]
    start_index <- match(period[[1]], dates)
    end_index <- match(period[[2]], dates)
    if (is.na(start_index) || is.na(end_index)) {
      stop("Contribution window is outside the estimated sample: ", episode)
    }
    if (start_index <= 1L) {
      stop("Contribution window needs one pre-period observation: ", episode)
    }
    pre_start_index <- start_index - 1L
    direction <- if (episode == "disinflation") -1 else 1
    keep <- dates >= period[[1]] & dates <= period[[2]]
    months <- sum(keep)
    observed_keep <- observed_dates >= period[[1]] & observed_dates <= period[[2]]
    if (!any(observed_dates == period[[1]]) || !any(observed_dates == period[[2]])) {
      stop("Observed inflation data do not cover the contribution window: ", episode)
    }
    if (any(!is.finite(monthly_inflation[observed_keep]))) {
      stop("Observed monthly inflation is not finite in contribution window: ", episode)
    }
    cumulative_inflation_pp <- 100 * sum(monthly_inflation[observed_keep])
    if (abs(cumulative_inflation_pp) < sqrt(.Machine$double.eps)) {
      stop("Cumulative inflation is zero in contribution window: ", episode)
    }

    for (shock in shocks) {
      level_sum <- sum(hicp[keep, shock], na.rm = TRUE)
      rows[[index]] <- data.frame(
        Country = country,
        Method = method,
        Episode = episode,
        Period = period_label(period),
        Shock = shock,
        Cumulative_inflation_contribution_pp = 100 * (
          hicp[end_index, shock] - hicp[pre_start_index, shock]
        ),
        Change_in_yoy_contribution_pp =
          yoy[end_index, shock] - yoy[start_index, shock],
        Episode_explained_yoy_change_pp = direction * (
          yoy[end_index, shock] - yoy[start_index, shock]
        ),
        Annual_inflation_change_pp = direction * (
          yoy[end_index, shock] - yoy[start_index, shock]
        ),
        Cumulative_inflation_pp = cumulative_inflation_pp,
        Average_yoy_contribution_pp = mean(yoy[keep, shock], na.rm = TRUE),
        Legacy_annualized_percent = 100 * (
          exp(level_sum)^(12 / months) - 1
        ),
        stringsAsFactors = FALSE
      )
      index <- index + 1L
    }
    rows[[index]] <- data.frame(
      Country = country,
      Method = method,
      Episode = episode,
      Period = period_label(period),
      Shock = "Total electricity",
      Cumulative_inflation_contribution_pp = 100 * (
        sum(hicp[end_index, ]) - sum(hicp[pre_start_index, ])
      ),
      Change_in_yoy_contribution_pp =
        sum(yoy[end_index, ]) - sum(yoy[start_index, ]),
      Episode_explained_yoy_change_pp = direction * (
        sum(yoy[end_index, ]) - sum(yoy[start_index, ])
      ),
      Annual_inflation_change_pp = direction * (
        sum(yoy[end_index, ]) - sum(yoy[start_index, ])
      ),
      Cumulative_inflation_pp = cumulative_inflation_pp,
      Average_yoy_contribution_pp = mean(rowSums(yoy[keep, , drop = FALSE])),
      Legacy_annualized_percent = 100 * (
        exp(sum(hicp[keep, , drop = FALSE]))^(12 / months) - 1
      ),
      stringsAsFactors = FALSE
    )
    index <- index + 1L
  }
  dplyr::bind_rows(rows) |>
    dplyr::group_by(Episode) |>
    dplyr::mutate(
      Contribution_log_points = Cumulative_inflation_contribution_pp / 100,
      Share_of_cumulative_inflation_percent = 100 *
        Cumulative_inflation_contribution_pp /
        Cumulative_inflation_pp,
      Share_of_contributed_inflation_percent =
        Share_of_cumulative_inflation_percent
    ) |>
    dplyr::ungroup()
}

export_contributions <- function(table, cfg, csv_name, tex_name,
                                 source_note = NULL) {
  save_csv(table, cfg, csv_name)
  note_text <- paste0(
    "*Contribution is the change in each shock's log-HICP level contribution between the month before the window and its final month; 0.01 log-point is approximately 1 percentage point of cumulative inflation. Share divides this contribution by observed cumulative inflation over the same window. Change_in_yoy_contribution_pp and Episode_explained_yoy_change_pp retain the former endpoint-based annual-inflation diagnostics; only the latter reverses sign for disinflation.*",
    if (is.null(source_note)) "" else paste0(" *", source_note, "*")
  )
  display <- table |>
    dplyr::transmute(
      Episode = dplyr::recode(
        Episode,
        inflation = "Inflation episode",
        disinflation = "Disinflation episode"
      ),
      Period,
      Shock,
      Contribution_log_points,
      Share_of_contributed_inflation_percent
    ) |>
    gt::gt(groupname_col = "Episode") |>
    gt::fmt_number(columns = Contribution_log_points, decimals = 4) |>
    gt::fmt_number(
      columns = Share_of_contributed_inflation_percent,
      decimals = 1
    ) |>
    gt::cols_label(
      Period = "Period",
      Shock = "Shock",
      Contribution_log_points = "Contribution (log-points)",
      Share_of_contributed_inflation_percent = "Share of cumulative inflation (%)"
    ) |>
    gt::tab_source_note(
      gt::md(note_text)
    )
  save_gt(display, cfg, tex_name)
  invisible(table)
}

combine_three_proxy_hd <- function(supply, demand, gas_supply) {
  dates <- lapply(list(supply, demand, gas_supply), function(x) as.Date(x$dates))
  if (!identical(dates[[1]], dates[[2]]) || !identical(dates[[1]], dates[[3]])) {
    stop("Supply, demand and gas-supply models must use the same dates.")
  }
  common_variables <- Reduce(
    intersect, list(supply$variables, demand$variables, gas_supply$variables)
  )
  hd <- array(
    NA_real_,
    dim = c(length(dates[[1]]), length(common_variables), 3L),
    dimnames = list(
      NULL, common_variables,
      c("Electricity supply", "Electricity demand", "Gas supply")
    )
  )
  hd[, , "Electricity supply"] <-
    supply$hd[, common_variables, 1, drop = FALSE][, , 1]
  hd[, , "Electricity demand"] <-
    demand$hd[, common_variables, 1, drop = FALSE][, , 1]
  hd[, , "Gas supply"] <-
    gas_supply$hd[, common_variables, 1, drop = FALSE][, , 1]
  list(hd = hd, dates = dates[[1]], variables = common_variables)
}

electricity_price_contribution_table <- function(decomposition, windows,
                                                 observed_data) {
  dates <- as.Date(decomposition$dates)
  shocks <- dimnames(decomposition$hd)[[3]]
  price_hd <- decomposition$hd[, "spot_price_log", , drop = FALSE][, 1, ]
  if (is.vector(price_hd)) price_hd <- matrix(price_hd, ncol = 1L)
  colnames(price_hd) <- shocks

  assert_columns(
    observed_data,
    c("observation_date", "spot_price_log"),
    "Spain observed electricity-price data"
  )
  observed_data <- observed_data[order(observed_data$observation_date), , drop = FALSE]
  observed_dates <- as.Date(observed_data$observation_date)

  rows <- list()
  index <- 1L
  for (episode in names(windows)) {
    period <- windows[[episode]]
    start_index <- match(period[[1]], dates)
    end_index <- match(period[[2]], dates)
    observed_start_index <- match(period[[1]], observed_dates)
    observed_end_index <- match(period[[2]], observed_dates)
    if (any(is.na(c(
      start_index, end_index, observed_start_index, observed_end_index
    )))) {
      stop(
        "Electricity-price contribution window is outside the sample: ",
        episode
      )
    }
    if (start_index <= 1L || observed_start_index <= 1L) {
      stop(
        "Electricity-price contribution window needs one pre-period: ",
        episode
      )
    }
    pre_start_index <- start_index - 1L
    observed_pre_start_index <- observed_start_index - 1L
    observed_change_pp <- 100 * (
      observed_data$spot_price_log[[observed_end_index]] -
        observed_data$spot_price_log[[observed_pre_start_index]]
    )
    if (abs(observed_change_pp) < sqrt(.Machine$double.eps)) {
      stop("Observed electricity-price change is zero in window: ", episode)
    }

    contributions <- 100 * (
      price_hd[end_index, ] - price_hd[pre_start_index, ]
    )
    identified_total <- sum(contributions)
    component_values <- c(
      "Observed electricity price change" = observed_change_pp,
      "Gas supply" = contributions[["Gas supply"]],
      "Electricity supply" = contributions[["Electricity supply"]],
      "Electricity demand" = contributions[["Electricity demand"]],
      "Other/unidentified factors" = observed_change_pp - identified_total
    )
    hierarchy_levels <- c(0L, 1L, 1L, 1L, 1L)
    for (component_index in seq_along(component_values)) {
      component <- names(component_values)[[component_index]]
      contribution_pp <- unname(component_values[[component_index]])
      rows[[index]] <- data.frame(
        Country = "Spain",
        Method = paste(
          "External instruments: electricity supply, electricity demand",
          "and Gazzani gas supply"
        ),
        Episode = episode,
        Period = period_label(period),
        Shock = component,
        Cumulative_electricity_price_contribution_pp = contribution_pp,
        Cumulative_electricity_price_change_pp = observed_change_pp,
        Contribution_log_points = contribution_pp / 100,
        Share_of_electricity_price_change_percent =
          100 * contribution_pp / observed_change_pp,
        Hierarchy_level = hierarchy_levels[[component_index]],
        stringsAsFactors = FALSE
      )
      index <- index + 1L
    }
  }
  dplyr::bind_rows(rows)
}

export_electricity_price_contributions <- function(table, cfg) {
  save_csv(
    table, cfg,
    "contributions_spain_electricity_price_three_shocks.csv"
  )
  display <- table |>
    dplyr::transmute(
      Episode = paste0(
        dplyr::recode(
          Episode,
          inflation = "Inflation episode",
          disinflation = "Disinflation episode"
        ),
        " (", Period, ")"
      ),
      Component = Shock,
      Contribution_log_points,
      Share_of_electricity_price_change_percent,
      Hierarchy_level
    ) |>
    gt::gt(groupname_col = "Episode", rowname_col = "Component") |>
    gt::tab_stub_indent(rows = Hierarchy_level == 1L, indent = 2L) |>
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_stub(rows = Component %in% c(
        "Gas supply", "Electricity supply", "Electricity demand"
      ))
    ) |>
    gt::cols_hide(columns = Hierarchy_level) |>
    gt::fmt_number(columns = Contribution_log_points, decimals = 4) |>
    gt::fmt_number(
      columns = Share_of_electricity_price_change_percent,
      decimals = 1
    ) |>
    gt::cols_label(
      Contribution_log_points = "Contribution (log-points)",
      Share_of_electricity_price_change_percent =
        "Share of electricity-price change (%)"
    )
  save_gt(
    display, cfg,
    "cumulative_contributions_electricity_price_three_shocks.tex"
  )
  invisible(table)
}
causal_filter <- function(impulse, innovations) {
  impulse <- as.numeric(impulse)
  innovations <- as.numeric(innovations)
  output <- numeric(length(innovations))
  for (tt in seq_along(innovations)) {
    available <- min(length(impulse), tt)
    horizons <- 0:(available - 1L)
    output[[tt]] <- sum(
      impulse[horizons + 1L] * innovations[tt - horizons]
    )
  }
  output
}

historical_decomposition_from_irf <- function(irf, shocks, variable_names) {
  shocks <- as.numeric(shocks)
  hd <- matrix(
    0, nrow = length(shocks), ncol = length(variable_names),
    dimnames = list(NULL, variable_names)
  )
  for (variable in seq_along(variable_names)) {
    hd[, variable] <- causal_filter(irf[, variable], shocks)
  }
  hd
}

irf_matrix_to_long <- function(irf, shock, channel) {
  data.frame(
    Horizon = rep(0:(nrow(irf) - 1L), each = ncol(irf)),
    Variable = rep(colnames(irf), times = nrow(irf)),
    Shock = shock,
    Estimate = as.vector(t(irf)),
    Channel = channel,
    stringsAsFactors = FALSE
  )
}

gas_supply_electricity_path <- function(supply, gas_supply) {
  if (!identical(as.Date(supply$dates), as.Date(gas_supply$dates)) ||
      !identical(supply$variables, gas_supply$variables)) {
    stop("Electricity- and gas-supply models must use the same VAR sample.")
  }
  variables <- gas_supply$variables
  price_index <- match("spot_price_log", variables)
  if (is.na(price_index)) stop("Electricity price is missing from the VAR.")
  horizon <- min(dim(supply$ma)[[3]], dim(gas_supply$ma)[[3]])

  gas_irf <- matrix(
    NA_real_, nrow = horizon, ncol = length(variables),
    dimnames = list(0:(horizon - 1L), variables)
  )
  supply_irf <- gas_irf
  for (hh in seq_len(horizon)) {
    gas_irf[hh, ] <- drop(
      gas_supply$ma[, , hh] %*% gas_supply$theta[, 1, drop = FALSE]
    )
    supply_irf[hh, ] <- drop(
      supply$ma[, , hh] %*% supply$theta[, 1, drop = FALSE]
    )
  }

  # Dynamic mediation: recover the sequence of electricity-supply shocks whose
  # convolution with the identified electricity-supply price IRF exactly
  # reproduces the electricity-price response to one gas-supply shock.
  gas_price_irf <- gas_irf[, price_index]
  supply_price_irf <- supply_irf[, price_index]
  if (abs(supply_price_irf[[1]]) < sqrt(.Machine$double.eps)) {
    stop("The electricity-supply price response is zero on impact.")
  }
  equivalent_supply_irf <- numeric(horizon)
  for (tt in seq_len(horizon)) {
    available <- tt - 1L
    inherited <- if (available > 0L) {
      horizons <- seq_len(available)
      sum(
        supply_price_irf[horizons + 1L] *
          equivalent_supply_irf[tt - horizons]
      )
    } else {
      0
    }
    equivalent_supply_irf[[tt]] <-
      (gas_price_irf[[tt]] - inherited) / supply_price_irf[[1]]
  }

  via_electricity_irf <- matrix(
    0, nrow = horizon, ncol = length(variables),
    dimnames = dimnames(gas_irf)
  )
  for (variable in seq_along(variables)) {
    for (tt in seq_len(horizon)) {
      horizons <- 0:(tt - 1L)
      via_electricity_irf[tt, variable] <- sum(
        supply_irf[horizons + 1L, variable] *
          equivalent_supply_irf[tt - horizons]
      )
    }
  }
  direct_gas_irf <- gas_irf - via_electricity_irf

  gas_shocks <- gas_supply$shocks[1, ]
  via_electricity_hd <- historical_decomposition_from_irf(
    via_electricity_irf, gas_shocks, variables
  )
  direct_gas_hd <- historical_decomposition_from_irf(
    direct_gas_irf, gas_shocks, variables
  )
  total_hd <- gas_supply$hd[, , 1, drop = FALSE][, , 1]
  reconstructed_price_irf <- causal_filter(
    supply_price_irf, equivalent_supply_irf
  )
  list(
    gas_irf = gas_irf,
    supply_irf = supply_irf,
    equivalent_supply_irf = equivalent_supply_irf,
    equivalent_supply_shocks = causal_filter(
      equivalent_supply_irf, gas_shocks
    ),
    via_electricity_irf = irf_matrix_to_long(
      via_electricity_irf, "Gas supply", "Via electricity market"
    ),
    direct_gas_irf = irf_matrix_to_long(
      direct_gas_irf, "Gas supply", "Direct gas and other non-electricity channels"
    ),
    total_hd = total_hd,
    via_electricity_hd = via_electricity_hd,
    direct_gas_hd = direct_gas_hd,
    without_electricity_hd = direct_gas_hd,
    price_irf_reconstruction_error = max(abs(
      reconstructed_price_irf - gas_price_irf
    )),
    max_abs_reconstruction_error = max(abs(
      total_hd - via_electricity_hd - direct_gas_hd
    ))
  )
}
historical_decomposition_long <- function(decomposition) {
  shocks <- dimnames(decomposition$hd)[[3]]
  grid <- expand.grid(
    Date = as.Date(decomposition$dates),
    Variable = decomposition$variables,
    Shock = shocks,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  grid$Contribution <- as.vector(decomposition$hd)
  grid
}

plot_three_shock_decomposition <- function(decomposition) {
  selected <- c("spot_price_log", "HICP_log")
  plot_data <- historical_decomposition_long(decomposition) |>
    dplyr::filter(Variable %in% selected) |>
    dplyr::mutate(
      Variable = factor(
        Variable, levels = selected,
        labels = vapply(selected, variable_label, character(1))
      )
    )
  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = Date, y = Contribution, colour = Shock, linetype = Shock)
  ) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey70", linewidth = 0.3) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::facet_wrap(~Variable, scales = "free_y", ncol = 1) +
    ggplot2::labs(x = NULL, y = "Log-point contribution", colour = NULL, linetype = NULL) +
    ggplot2::guides(
      colour = ggplot2::guide_legend(nrow = 1, byrow = TRUE),
      linetype = ggplot2::guide_legend(nrow = 1, byrow = TRUE)
    )
}

combine_extended_gas_channel_hd <- function(supply, demand, gas_path) {
  if (!identical(as.Date(supply$dates), as.Date(demand$dates))) {
    stop("Supply and demand models must use the same dates.")
  }
  common_variables <- intersect(supply$variables, demand$variables)
  hd <- array(
    NA_real_,
    dim = c(length(supply$dates), length(common_variables), 4L),
    dimnames = list(
      NULL, common_variables,
      c(
        "Electricity supply", "Electricity demand",
        "Gas via electricity", "Direct gas effect"
      )
    )
  )
  hd[, , "Electricity supply"] <-
    supply$hd[, common_variables, 1, drop = FALSE][, , 1]
  hd[, , "Electricity demand"] <-
    demand$hd[, common_variables, 1, drop = FALSE][, , 1]
  hd[, , "Gas via electricity"] <-
    gas_path$via_electricity_hd[, common_variables, drop = FALSE]
  hd[, , "Direct gas effect"] <-
    gas_path$direct_gas_hd[, common_variables, drop = FALSE]
  list(hd = hd, dates = supply$dates, variables = common_variables)
}

plot_extended_gas_channel_decomposition <- function(decomposition) {
  selected <- c("spot_price_log", "HICP_log")
  plot_data <- historical_decomposition_long(decomposition) |>
    dplyr::filter(Variable %in% selected) |>
    dplyr::mutate(
      Variable = factor(
        Variable, levels = selected,
        labels = vapply(selected, variable_label, character(1))
      )
    )
  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = Date, y = Contribution, fill = Shock)
  ) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey35", linewidth = 0.3) +
    ggplot2::geom_col(width = 25, position = "stack") +
    ggplot2::facet_wrap(~Variable, scales = "free_y", ncol = 1) +
    ggplot2::labs(x = NULL, y = "Log-point contribution", fill = NULL) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1, byrow = TRUE))
}
energy_contribution_table <- function(supply, demand, gas_supply, gas_path,
                                      windows, observed_data) {
  dates <- lapply(list(supply, demand, gas_supply), function(x) as.Date(x$dates))
  if (!identical(dates[[1]], dates[[2]]) || !identical(dates[[1]], dates[[3]])) {
    stop("Electricity supply, demand and gas-supply models need the same dates.")
  }
  common_variables <- Reduce(
    intersect, list(supply$variables, demand$variables, gas_supply$variables)
  )
  electricity_supply <- supply$hd[, common_variables, 1, drop = FALSE][, , 1]
  electricity_demand <- demand$hd[, common_variables, 1, drop = FALSE][, , 1]
  gas_total <- gas_supply$hd[, common_variables, 1, drop = FALSE][, , 1]
  gas_via_electricity <- gas_path$via_electricity_hd[, common_variables, drop = FALSE]
  gas_direct <- gas_path$direct_gas_hd[, common_variables, drop = FALSE]
  components <- array(
    NA_real_,
    dim = c(nrow(electricity_supply), length(common_variables), 5L),
    dimnames = list(
      NULL, common_variables,
      c(
        "Gas supply", "of which: direct gas effect",
        "of which: gas supply via electricity price",
        "Electricity supply", "Electricity demand"
      )
    )
  )
  components[, , "Gas supply"] <- gas_total
  components[, , "of which: direct gas effect"] <- gas_direct
  components[, , "of which: gas supply via electricity price"] <- gas_via_electricity
  components[, , "Electricity supply"] <- electricity_supply
  components[, , "Electricity demand"] <- electricity_demand
  decomposition <- list(
    hd = components,
    dates = supply$dates,
    variables = common_variables
  )
  base <- contribution_table_from_hd(
    decomposition, windows, "Spain",
    paste(
      "External instruments: electricity supply, electricity demand",
      "and Gazzani gas supply"
    ),
    observed_data
  ) |>
    dplyr::filter(Shock != "Total electricity")

  rows <- lapply(unique(base$Episode), function(episode) {
    episode_rows <- base[base$Episode == episode, , drop = FALSE]
    get_row <- function(shock) {
      row <- episode_rows[episode_rows$Shock == shock, , drop = FALSE]
      if (nrow(row) != 1L) stop("Missing energy contribution row: ", shock)
      row
    }
    gas <- get_row("Gas supply")
    direct <- get_row("of which: direct gas effect")
    via <- get_row("of which: gas supply via electricity price")
    elec_supply <- get_row("Electricity supply")
    elec_demand <- get_row("Electricity demand")
    observed <- gas
    other <- gas
    diagnostic_columns <- c(
      "Change_in_yoy_contribution_pp",
      "Episode_explained_yoy_change_pp",
      "Annual_inflation_change_pp",
      "Average_yoy_contribution_pp",
      "Legacy_annualized_percent"
    )
    observed[diagnostic_columns] <- NA_real_
    other[diagnostic_columns] <- NA_real_
    observed$Cumulative_inflation_contribution_pp <-
      observed$Cumulative_inflation_pp
    other$Cumulative_inflation_contribution_pp <-
      observed$Cumulative_inflation_pp -
      gas$Cumulative_inflation_contribution_pp -
      elec_supply$Cumulative_inflation_contribution_pp -
      elec_demand$Cumulative_inflation_contribution_pp
    observed$Contribution_log_points <-
      observed$Cumulative_inflation_contribution_pp / 100
    other$Contribution_log_points <-
      other$Cumulative_inflation_contribution_pp / 100
    observed$Share_of_cumulative_inflation_percent <- 100
    observed$Share_of_contributed_inflation_percent <- 100
    other$Share_of_cumulative_inflation_percent <- 100 *
      other$Cumulative_inflation_contribution_pp /
      other$Cumulative_inflation_pp
    other$Share_of_contributed_inflation_percent <-
      other$Share_of_cumulative_inflation_percent
    observed$Shock <- "Observed inflation"
    gas$Shock <- "Gas supply"
    direct$Shock <- "of which: direct effect"
    via$Shock <- "of which: transit through electricity"
    elec_supply$Shock <- "Electricity supply"
    elec_demand$Shock <- "Electricity demand"
    other$Shock <- "Other/unidentified factors"
    result <- dplyr::bind_rows(
      observed, gas, direct, via, elec_supply, elec_demand, other
    )
    result$Hierarchy_level <- c(0L, 1L, 2L, 2L, 1L, 1L, 1L)
    result
  })
  dplyr::bind_rows(rows)
}

export_energy_contributions <- function(table, cfg) {
  save_csv(table, cfg, "contributions_spain_energy_three_shocks.csv")
  display <- table |>
    dplyr::transmute(
      Episode = paste0(
        dplyr::recode(
          Episode,
          inflation = "Inflation episode",
          disinflation = "Disinflation episode"
        ),
        " (", Period, ")"
      ),
      Component = Shock,
      Contribution_log_points,
      Share_of_contributed_inflation_percent = dplyr::if_else(
        startsWith(Episode, "Disinflation episode"),
        NA_real_,
        Share_of_contributed_inflation_percent
      ),
      Hierarchy_level
    ) |>
    gt::gt(groupname_col = "Episode", rowname_col = "Component") |>
    gt::tab_stub_indent(rows = Hierarchy_level == 1L, indent = 2L) |>
    gt::tab_stub_indent(rows = Hierarchy_level == 2L, indent = 4L) |>
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_stub(rows = Component %in% c(
        "Gas supply", "Electricity supply", "Electricity demand"
      ))
    ) |>
    gt::cols_hide(columns = Hierarchy_level) |>
    gt::fmt_number(columns = Contribution_log_points, decimals = 4) |>
    gt::fmt_number(
      columns = Share_of_contributed_inflation_percent,
      decimals = 1
    ) |>
    gt::sub_missing(
      columns = Share_of_contributed_inflation_percent,
      missing_text = ""
    ) |>
    gt::cols_label(
      Contribution_log_points = "Contribution (log-points)",
      Share_of_contributed_inflation_percent =
        "Share of cumulative inflation (%)"
    ) |>
    gt::tab_source_note(gt::md(paste(
      "*Observed inflation is decomposed into the four level-one components:",
      "gas supply, electricity supply, electricity demand and other/unidentified",
      "factors. The two level-two rows decompose gas supply into its direct effect",
      "and its transit through electricity; they are not added again. Contributions",
      "are changes in shock-specific log-HICP levels between the month before the",
      "window and its final month.*"
    )))
  save_gt(
    display, cfg,
    "cumulative_contributions_energy_three_shocks.tex"
  )
  invisible(table)
}
export_combined_three_shock_contributions <- function(
    electricity_table, energy_table, cfg) {
  backslash <- intToUtf8(92L)
  command <- function(text) paste0(backslash, text)
  row_line <- function(cells, suffix = "") {
    paste0(paste(cells, collapse = " & "), " ", backslash, backslash, suffix)
  }
  get_row <- function(table, episode, shock) {
    row <- table[
      table$Episode == episode & table$Shock == shock,
      ,
      drop = FALSE
    ]
    if (nrow(row) != 1L) {
      stop("Combined table requires one row for ", episode, ": ", shock)
    }
    row
  }
  component_label <- function(label, hierarchy, bold) {
    if (identical(label, "Other/unidentified factors")) label <- "Other"
    indent <- switch(
      as.character(hierarchy),
      "1" = paste0(command("hspace*{7.5pt}"), " "),
      "2" = paste0(command("hspace*{15pt}"), " "),
      ""
    )
    output <- paste0(indent, label)
    if (bold) paste0("{", command("bfseries "), output, "}") else output
  }
  side_cells <- function(row, share_column, hierarchy, bold,
                         suppress_share = FALSE) {
    if (is.null(row)) return(c("", "", ""))
    share <- if (suppress_share) {
      ""
    } else {
      formatC(row[[share_column]][[1]], format = "f", digits = 1)
    }
    c(
      component_label(row$Shock[[1]], hierarchy, bold),
      formatC(row$Contribution_log_points[[1]], format = "f", digits = 4),
      share
    )
  }

  row_layout <- list(
    list(
      electricity = "Observed electricity price change",
      energy = "Observed inflation",
      electricity_level = 0L, energy_level = 0L,
      electricity_bold = FALSE, energy_bold = FALSE
    ),
    list(
      electricity = "Gas supply", energy = "Gas supply",
      electricity_level = 1L, energy_level = 1L,
      electricity_bold = TRUE, energy_bold = TRUE
    ),
    list(
      electricity = NULL, energy = "of which: direct effect",
      electricity_level = 0L, energy_level = 2L,
      electricity_bold = FALSE, energy_bold = FALSE
    ),
    list(
      electricity = NULL, energy = "of which: transit through electricity",
      electricity_level = 0L, energy_level = 2L,
      electricity_bold = FALSE, energy_bold = FALSE
    ),
    list(
      electricity = "Electricity supply", energy = "Electricity supply",
      electricity_level = 1L, energy_level = 1L,
      electricity_bold = TRUE, energy_bold = TRUE
    ),
    list(
      electricity = "Electricity demand", energy = "Electricity demand",
      electricity_level = 1L, energy_level = 1L,
      electricity_bold = TRUE, energy_bold = TRUE
    ),
    list(
      electricity = "Other/unidentified factors",
      energy = "Other/unidentified factors",
      electricity_level = 1L, energy_level = 1L,
      electricity_bold = FALSE, energy_bold = FALSE
    )
  )

  output <- c(
    command("begin{table}[!t]"),
    command("centering"),
    command("resizebox{"),
    paste0(backslash, "linewidth}{!}{%"),
    command("begin{tabular}{lrrlrr}"),
    command("toprule"),
    row_line(c(
      command("multicolumn{3}{c}{"),
      command("multicolumn{3}{c}{")
    ))
  )
  # Replace the deliberately split multicolumn placeholders with complete cells.
  output[[length(output)]] <- row_line(c(
    paste0(command("multicolumn{3}{c}{"), command("textbf{Electricity price}}")),
    paste0(command("multicolumn{3}{c}{"), command("textbf{HICP}}"))
  ))
  output <- c(
    output,
    row_line(c(
      "Component", "Contribution (log-points)",
      paste0("Share (", backslash, "%)"),
      "Component", "Contribution (log-points)",
      paste0("Share (", backslash, "%)")
    )),
    command("midrule")
  )

  for (episode in c("inflation", "disinflation")) {
    electricity_episode <- electricity_table[
      electricity_table$Episode == episode, , drop = FALSE
    ]
    energy_episode <- energy_table[
      energy_table$Episode == episode, , drop = FALSE
    ]
    episode_name <- if (episode == "inflation") {
      "Inflation episode"
    } else {
      "Disinflation episode"
    }
    output <- c(
      output,
      row_line(
        c(
          paste0(
            command("multicolumn{3}{l}{"),
            episode_name, " (", electricity_episode$Period[[1]], ")}"
          ),
          paste0(
            command("multicolumn{3}{l}{"),
            episode_name, " (", energy_episode$Period[[1]], ")}"
          )
        ),
        "[2.5pt]"
      ),
      command("midrule")
    )
    for (layout in row_layout) {
      electricity_row <- if (is.null(layout$electricity)) {
        NULL
      } else {
        get_row(electricity_table, episode, layout$electricity)
      }
      energy_row <- get_row(energy_table, episode, layout$energy)
      output <- c(
        output,
        row_line(c(
          side_cells(
            electricity_row,
            "Share_of_electricity_price_change_percent",
            layout$electricity_level,
            layout$electricity_bold
          ),
          side_cells(
            energy_row,
            "Share_of_contributed_inflation_percent",
            layout$energy_level,
            layout$energy_bold,
            suppress_share = episode == "disinflation"
          )
        ))
      )
    }
    if (episode == "inflation") {
      output <- c(output, command("midrule"))
    }
  }
  output <- c(
    output,
    command("bottomrule"),
    command("end{tabular}"),
    "}",
    command("caption{Cumulative contributions of electricity-demand, electricity-supply, and gas-supply shocks}"),
    command("label{tab:cumulative-contributions-electricity-energy-three-shocks}"),
    command("end{table}")
  )
  filename <- "cumulative_contributions_electricity_energy_three_shocks.tex"
  internal <- file.path(cfg$table_dir, filename)
  writeLines(output, internal, useBytes = TRUE)
  if (cfg$overleaf$enabled) {
    file.copy(
      internal,
      file.path(cfg$overleaf$tables, filename),
      overwrite = TRUE
    )
  }
  message("Saved table: ", internal)
  invisible(internal)
}
run_spain_contributions <- function(supply, demand, gas_supply, cfg,
                                    observed_data) {
  benchmark <- combine_single_proxy_hd(supply, demand)
  benchmark_table <- contribution_table_from_hd(
    benchmark,
    cfg$windows,
    country = "Spain",
    method = "External instruments: benchmark PCA proxies",
    observed_data = observed_data
  )
  export_contributions(
    benchmark_table, cfg,
    "contributions_spain.csv",
    "cumulative_contributions.tex"
  )

  three_shock <- combine_three_proxy_hd(supply, demand, gas_supply)
  save_csv(
    historical_decomposition_long(three_shock), cfg,
    "historical_decomposition_spain_three_shocks.csv"
  )
  save_plot(
    plot_three_shock_decomposition(three_shock), cfg,
    "historical_decomposition_spain_three_shocks.png",
    overleaf_dir = "macro", width = 10, height = 10,
    theme_overrides = ggplot2::theme(legend.position = "bottom")
  )

  electricity_price_table <- electricity_price_contribution_table(
    three_shock, cfg$electricity_price_windows, observed_data
  )
  export_electricity_price_contributions(electricity_price_table, cfg)

  gas_path <- gas_supply_electricity_path(supply, gas_supply)
  extended_gas_channels <- combine_extended_gas_channel_hd(
    supply, demand, gas_path
  )
  save_csv(
    historical_decomposition_long(extended_gas_channels), cfg,
    "historical_decomposition_spain_extended_gas_channels.csv"
  )
  save_plot(
    plot_extended_gas_channel_decomposition(extended_gas_channels), cfg,
    "historical_decomposition_spain_extended_gas_channels.png",
    overleaf_dir = "macro", width = 12, height = 10,
    theme_overrides = ggplot2::theme(legend.position = "bottom")
  )
  save_csv(
    data.frame(
      Date = as.Date(gas_supply$dates),
      Gas_supply_structural_shock = as.numeric(gas_supply$shocks[1, ]),
      Induced_electricity_supply_shock = gas_path$equivalent_supply_shocks
    ),
    cfg, "historical_gas_induced_electricity_supply_shocks.csv"
  )
  gas_channel_hd <- array(
    NA_real_,
    dim = c(length(gas_supply$dates), length(gas_supply$variables), 3L),
    dimnames = list(
      NULL, gas_supply$variables,
      c("Gas supply total", "Gas via electricity", "Direct gas effect")
    )
  )
  gas_channel_hd[, , "Gas supply total"] <- gas_path$total_hd
  gas_channel_hd[, , "Gas via electricity"] <- gas_path$via_electricity_hd
  gas_channel_hd[, , "Direct gas effect"] <- gas_path$direct_gas_hd
  save_csv(
    historical_decomposition_long(list(
      hd = gas_channel_hd,
      dates = gas_supply$dates,
      variables = gas_supply$variables
    )),
    cfg, "historical_decomposition_spain_gas_channels.csv"
  )
  save_csv(
    dplyr::bind_rows(
      gas_path$via_electricity_irf,
      gas_path$direct_gas_irf
    ),
    cfg, "irfs_gas_supply_path_decomposition.csv"
  )
  energy_table <- energy_contribution_table(
    supply, demand, gas_supply, gas_path, cfg$windows, observed_data
  )
  export_energy_contributions(energy_table, cfg)
  export_combined_three_shock_contributions(
    electricity_price_table, energy_table, cfg
  )
  save_csv(
    data.frame(
      Metric = c(
        "Gas-price impact normalization",
        "Electricity-price impact of gas-supply shock",
        "Equivalent electricity-supply shock on impact",
        "Equivalent electricity-supply shock minimum over IRF horizon",
        "Equivalent electricity-supply shock maximum over IRF horizon",
        "Gas-induced electricity-price IRF: max reconstruction error",
        "Gas total = via electricity + direct gas: max HD error"
      ),
      Value = c(
        gas_supply$theta["spot_gas_log", 1],
        gas_supply$theta["spot_price_log", 1],
        gas_path$equivalent_supply_irf[[1]],
        min(gas_path$equivalent_supply_irf),
        max(gas_path$equivalent_supply_irf),
        gas_path$price_irf_reconstruction_error,
        gas_path$max_abs_reconstruction_error
      )
    ),
    cfg, "gas_supply_path_decomposition_diagnostics.csv"
  )
  list(
    decomposition = benchmark,
    table = benchmark_table,
    three_shock_decomposition = three_shock,
    gas_path = gas_path,
    energy_table = energy_table,
    electricity_price_table = electricity_price_table
  )
}
