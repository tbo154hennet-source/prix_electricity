`%||%` <- function(x, y) if (is.null(x)) y else x

required_packages <- c(
  "arrow", "dplyr", "ggplot2", "gt", "MASS",
  "readr", "readxl", "tidyr", "vars"
)

check_dependencies <- function(use_inference = TRUE) {
  packages <- required_packages
  if (use_inference) packages <- c(packages, "varexternal")
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    stop(
      "Missing R packages: ", paste(missing, collapse = ", "),
      ". Install them before running the pipeline."
    )
  }
  if (use_inference) {
    suppressPackageStartupMessages(
      library("varexternal", character.only = TRUE)
    )
  }
  invisible(TRUE)
}

ensure_directories <- function(cfg) {
  dirs <- c(
    cfg$output_dir, cfg$model_dir, cfg$table_dir,
    cfg$figure_dir, cfg$log_dir
  )
  if (cfg$overleaf$enabled) {
    dirs <- c(dirs, cfg$overleaf$macro, cfg$overleaf$policy, cfg$overleaf$tables)
  }
  invisible(lapply(unique(dirs), dir.create, recursive = TRUE, showWarnings = FALSE))
}

pick_column <- function(data, candidates, label, country) {
  found <- candidates[candidates %in% names(data)]
  if (!length(found)) {
    stop(
      "Missing ", label, " for ", country, ". Tried: ",
      paste(candidates, collapse = ", ")
    )
  }
  found[[1]]
}

assert_columns <- function(data, columns, context) {
  missing <- setdiff(columns, names(data))
  if (length(missing)) {
    stop(context, " is missing: ", paste(missing, collapse = ", "))
  }
  invisible(TRUE)
}

# External instruments aligned with the VAR residuals. The VAR sample can start
# before an instrument is observed (2010 versus 2015 for the micro proxies):
# unobserved months are set to 0, which adds nothing to the moments
# sum(z_t u_t), and `observed` keeps them out of first stages and centering.
proxy_instrument <- function(sample, proxy, p) {
  z <- as.matrix(sample[, proxy, drop = FALSE])[-seq_len(p), , drop = FALSE]
  observed <- rowSums(is.finite(z)) > 0
  z[!is.finite(z)] <- 0
  list(z = if (ncol(z) == 1L) drop(z) else z, observed = observed)
}

finite_complete <- function(data, columns) {
  assert_columns(data, columns, "Model data")
  keep <- stats::complete.cases(data[, columns, drop = FALSE])
  numeric_columns <- columns[vapply(data[, columns, drop = FALSE], is.numeric, logical(1))]
  if (length(numeric_columns)) {
    keep <- keep & apply(
      data[, numeric_columns, drop = FALSE],
      1, function(row) all(is.finite(row))
    )
  }
  data[keep, , drop = FALSE]
}

quarter_label <- function(date) {
  paste0(
    format(date, "%Y"), "Q",
    ((as.integer(format(date, "%m")) - 1L) %/% 3L) + 1L
  )
}

period_label <- function(period) {
  paste(quarter_label(period[[1]]), quarter_label(period[[2]]), sep = "--")
}

variable_label <- function(variable) {
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
  unname(labels[variable]) %||% variable
}

cache_path <- function(cfg, name) {
  file.path(cfg$model_dir, paste0(name, ".rds"))
}

cache_model <- function(cfg, name, builder) {
  path <- cache_path(cfg, name)
  if (!cfg$force && file.exists(path)) {
    cached <- readRDS(path)
    incomplete_msw <- grepl("_msw$", name) &&
      identical(cached$method, "External instrument") &&
      is.null(cached$inference_irfs)
    if (!incomplete_msw) {
      message("Using cached model: ", name)
      return(cached)
    }
    message("Rebuilding incomplete MSW cache: ", name)
  }
  value <- builder()
  saveRDS(value, path)
  value
}

save_csv <- function(data, cfg, filename) {
  path <- file.path(cfg$table_dir, filename)
  readr::write_csv(data, path)
  message("Saved table: ", path)
  invisible(path)
}

save_plot <- function(plot, cfg, filename, overleaf_dir = "macro",
                      width = 10, height = 6, theme_overrides = NULL) {
  plot <- plot +
    ggplot2::theme_minimal(base_size = 20) +
    ggplot2::labs(title = NULL, subtitle = NULL)
  if (!is.null(theme_overrides)) {
    plot <- plot + theme_overrides
  }
  internal <- file.path(cfg$figure_dir, filename)
  ggplot2::ggsave(internal, plot = plot, width = width, height = height, dpi = 300)
  if (cfg$overleaf$enabled) {
    destination <- file.path(cfg$overleaf[[overleaf_dir]], filename)
    ggplot2::ggsave(
      destination, plot = plot, width = width, height = height, dpi = 300
    )
  }
  message("Saved plot: ", filename)
  invisible(internal)
}

save_gt <- function(table, cfg, filename) {
  internal <- file.path(cfg$table_dir, filename)
  gt::gtsave(table, internal)
  if (cfg$overleaf$enabled) {
    gt::gtsave(table, file.path(cfg$overleaf$tables, filename))
  }
  invisible(internal)
}

stage_log <- function(cfg, stage, expression) {
  started <- Sys.time()
  message("\n[", stage, "] started")
  value <- force(expression)
  elapsed <- round(as.numeric(difftime(Sys.time(), started, units = "secs")), 2)
  line <- paste(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), stage, elapsed, "seconds")
  cat(
    line, "\n",
    file = file.path(cfg$log_dir, "pipeline.log"),
    append = TRUE
  )
  message("[", stage, "] completed in ", elapsed, " seconds")
  value
}
