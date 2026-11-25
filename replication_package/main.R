command_line <- commandArgs(trailingOnly = FALSE)
file_argument <- grep("^--file=", command_line, value = TRUE)
if (!length(file_argument)) stop("Run this entry point with Rscript main.R")

main_file <- normalizePath(
  sub("^--file=", "", file_argument[[1]]),
  winslash = "/",
  mustWork = TRUE
)
replication_root <- dirname(main_file)
replication_output <- file.path(replication_root, "output")
Sys.setenv(
  REPLICATION_ROOT = replication_root,
  REPLICATION_OUTPUT_DIR = replication_output,
  VAR_EXPORT_OVERLEAF = "false"
)

arguments <- commandArgs(trailingOnly = TRUE)
check_only <- "--check-only" %in% arguments
stage_argument <- grep("^--stage=", arguments, value = TRUE)
selected <- if (length(stage_argument)) {
  strsplit(sub("^--stage=", "", stage_argument[[1]]), ",", fixed = TRUE)[[1]]
} else {
  "all"
}
valid <- c("all", "descriptive", "elasticity", "intraday", "macro")
invalid <- setdiff(selected, valid)
if (length(invalid)) stop("Unknown stage(s): ", paste(invalid, collapse = ", "))
if ("all" %in% selected) selected <- valid[-1]

dir.create(replication_output, recursive = TRUE, showWarnings = FALSE)
for (subdir in c(
  "cross_border", "data_description", "elasticity", "intra_day",
  "latex_tables", "macro", "policy", "supply_demand_curve"
)) {
  dir.create(file.path(replication_output, subdir), recursive = TRUE,
             showWarnings = FALSE)
}

manifest_path <- file.path(replication_root, "DATA_MANIFEST.csv")
manifest <- utils::read.csv(manifest_path, stringsAsFactors = FALSE,
                            check.names = FALSE)
required <- manifest[manifest$required_for_default == "yes", , drop = FALSE]
missing <- required$path[!file.exists(file.path(replication_root, required$path))]
if (length(missing)) {
  stop(
    "Missing replication inputs. Extract data/replication_data.zip at the ",
    "package root. Missing:\n- ", paste(missing, collapse = "\n- ")
  )
}
message("Input check passed: ", nrow(required), " required paths found.")
if (check_only) quit(status = 0)

run_r <- function(relative_path) {
  path <- file.path(replication_root, relative_path)
  message("Running ", relative_path)
  old <- getwd()
  on.exit(setwd(old), add = TRUE)
  setwd(replication_root)
  source(path, local = new.env(parent = globalenv()), chdir = FALSE)
}

run_rmd <- function(relative_path) {
  if (!requireNamespace("knitr", quietly = TRUE)) {
    stop("Package 'knitr' is required to execute ", relative_path)
  }
  path <- file.path(replication_root, relative_path)
  extracted <- tempfile(fileext = ".R")
  on.exit(unlink(extracted), add = TRUE)
  knitr::purl(path, output = extracted, documentation = 0, quiet = TRUE)
  message("Running ", relative_path)
  old <- getwd()
  on.exit(setwd(old), add = TRUE)
  setwd(replication_root)
  source(extracted, local = new.env(parent = globalenv()), chdir = FALSE)
}

# The archive already contains every required ENTSO-E extract. Uncomment only
# to rebuild it from the API (ENTSOE_API_KEY must be set first).
# system2(Sys.which("python"), c(file.path(replication_root, "entsoe", "download_entsoe.py"), "--dataset", "all", "--output", file.path(replication_root, "data", "entsoe")))

if ("descriptive" %in% selected) {
  run_rmd("supply_demand_curve/notebooks/plot_supply_demand_curve_hiver.Rmd")
  run_rmd("supply_demand_curve/notebooks/plot_reg_qty_final.Rmd")
  run_r("supply_demand_curve/estimate_elasticity/required_legacy/describe_synthetic_shocks.R")
  run_r("supply_demand_curve/estimate_elasticity/required_legacy/describe_synthetic_shocks_log_normalized.R")
  run_rmd("day_ahead/notebooks/describe_shocks.Rmd")
  run_rmd("day_ahead/notebooks/plot_crossborder_adj.Rmd")
  run_r("day_ahead/cross_border_data/out_of_sample_prediction_check.R")
}

if ("elasticity" %in% selected) {
  run_r("supply_demand_curve/distribution_elasticity/elasticity_comparison.R")
  run_r("supply_demand_curve/distribution_elasticity/heterogeneity_elasticity.R")
  run_r("supply_demand_curve/distribution_elasticity/robustness_pas_reg.R")
  run_r("supply_demand_curve/linear_reg_elas/elasticity_comparison_lin.R")
}

if ("intraday" %in% selected) {
  run_r("Intra_day_micro/elasticity/estimate_elasticity_intraday_reg.R")
  run_r("Intra_day_micro/elasticity/plot_all_elasticity.R")
  run_r("Intra_day_micro/elasticity/demand_elasticity_plot.R")
}

if ("macro" %in% selected) {
  rscript <- file.path(R.home("bin"), "Rscript")
  status <- system2(rscript, file.path(replication_root, "VAR_analysis", "main.R"))
  if (!identical(status, 0L)) stop("The macro pipeline failed with status ", status)
  run_r("VAR_analysis/supplementary/hydro_arbitrage.R")
}

message("Replication stages completed: ", paste(selected, collapse = ", "))
