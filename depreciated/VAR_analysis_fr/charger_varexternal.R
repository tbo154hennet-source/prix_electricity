my_lib <- "C:/Users/DBT3KD/AppData/Local/R/win-library/4.5"
zip_path <- "Z:/inceritude_oil/varexternal-master.zip"

dir.exists(dirname(zip_path))
file.exists(zip_path)

dir.create(my_lib, recursive = TRUE, showWarnings = FALSE)

.libPaths(c(my_lib, .libPaths()))

# Installer remotes si nécessaire
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes", lib = my_lib)
}

# Supprimer une ancienne installation éventuelle
target <- file.path(my_lib, "varexternal")
if (dir.exists(target)) unlink(target, recursive = TRUE, force = TRUE)

# Installer depuis le fichier .zip local
remotes::install_local(
  path = zip_path,
  lib = my_lib,
  upgrade = "never"
)

# Charger le package
library(varexternal, lib.loc = my_lib)