command_line <- commandArgs(trailingOnly = FALSE)
file_argument <- grep("^--file=", command_line, value = TRUE)
if (!length(file_argument)) {
  stop("Run this entry point with Rscript VAR_analysis/main.R")
}
main_file <- normalizePath(
  sub("^--file=", "", file_argument[[1]]),
  winslash = "/",
  mustWork = TRUE
)
pipeline_dir <- dirname(main_file)
root_dir <- dirname(pipeline_dir)

source(file.path(pipeline_dir, "config.R"), local = FALSE)
module_files <- sort(list.files(
  file.path(pipeline_dir, "R"),
  pattern = "\\.R$",
  full.names = TRUE
))
invisible(lapply(module_files, source, local = FALSE))

arguments <- commandArgs(trailingOnly = TRUE)
if ("--help" %in% arguments || "-h" %in% arguments) {
  cat(
    "Usage: Rscript VAR_analysis/main.R [--stage=all|irfs|relevance|contributions|counterfactuals|robustness|france]\n",
    "Several stages can be comma-separated. Dependencies are loaded automatically.\n",
    "Environment variables:\n",
    "  VAR_EXPORT_OVERLEAF=false  Disable writes to <external Overleaf directory>\n",
    "  VAR_USE_MSW_INFERENCE=false  Run point estimates only\n",
    "  VAR_FORCE=true  Ignore cached RDS models and re-estimate\n",
    "  VAR_LAGS=12, VAR_HORIZON=40, VAR_SEED=20260914\n",
    "  VAR_CF_BOOT=200, VAR_CF_BLOCK=0  Wind-counterfactual bootstrap\n",
    sep = ""
  )
  quit(status = 0)
}
stage_argument <- grep("^--stage=", arguments, value = TRUE)
selected_stages <- if (length(stage_argument)) {
  sub("^--stage=", "", stage_argument[[1]])
} else {
  "all"
}
valid_stages <- c(
  "all", "irfs", "relevance", "contributions",
  "counterfactuals", "robustness", "france"
)
requested <- unlist(strsplit(selected_stages, ",", fixed = TRUE))
invalid <- setdiff(requested, valid_stages)
if (length(invalid)) {
  stop("Unknown stage(s): ", paste(invalid, collapse = ", "))
}

cfg <- make_config(root_dir)
check_dependencies(cfg$use_inference)
ensure_directories(cfg)
message("VAR pipeline root: ", cfg$pipeline_dir)
message("Stages: ", selected_stages)
message("Overleaf export: ", cfg$overleaf$enabled)
run_pipeline(cfg, selected_stages)
message("Pipeline completed successfully.")
