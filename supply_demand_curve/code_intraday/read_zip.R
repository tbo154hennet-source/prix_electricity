


# Set working directory to your Desktop
setwd("//localhost/c$/Users/DBT3KD/Desktop")

# List all ZIP files on the Desktop
zip_files <- list.files(pattern = "\\.zip$", ignore.case = TRUE)
# Directory where you want to extract
output_dir <- "F:/intraday_supply_demand_curves"

# Loop through each ZIP file and extract
for (z in zip_files) {
  print(z)
  # Create output folder with the same name as ZIP (without .zip)
  # out_dir <- file.path(getwd(), tools::file_path_sans_ext(z))
  # if (!dir.exists(out_dir)) dir.create(out_dir)
  
  # Extract the ZIP contents
  unzip(zipfile = z, exdir = output_dir)
}

setwd(output_dir)
print(length(list.files(output_dir)))
      



#curve = "curva_pibc_2023053004.1"
#type ="demand"


