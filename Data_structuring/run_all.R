# main.R with error handling

run_script <- function(file) {
  tryCatch(
    {
      cat(paste0("Running ", file, "...\n"))
      source(file)
      cat(paste0(file, " completed successfully.\n"))
    },
    error = function(e) {
      cat(paste0("Error in ", file, ": ", e$message, "\n"))
      stop("Stopping execution due to error.")
    }
  )
}


setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Data_structuring")

# Step 1: Run the first script
run_script("read_IIP_plant_data.R")

# Step 2: Run the second script
run_script("read_IIP_plant_follow_up.R")

# Step 3: Run the third script
run_script("read_macro_data.Rmd")

cat("All scripts executed successfully.\n")