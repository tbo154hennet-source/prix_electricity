# Dossier où sont tes scripts
folder <- "Z:/prix_electricite_gitlab"
getwd()
# Liste des fichiers R dans le dossier (et sous-dossiers si besoin)
files <- list.files(
  folder,
  pattern = "\\.(R|Rmd)$",   # matches .R or .Rmd
  full.names = TRUE,
  recursive = TRUE
)

# Texte à remplacer
old_text <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab"
new_text <- "Z:/prix_electricite_gitlab"

files <- c("Z:/prix_electricite_gitlab/VAR_analysis_fr/supply_demand/irf_plot_esp_1var_bis.Rmd" )

# Remplacement dans tous les fichiers
for (f in files) {
  content <- readLines(f, warn = FALSE)
  content <- gsub(old_text, new_text, content, fixed = TRUE)
  writeLines(content, f)
}



files <- list.files(
  folder,
  pattern = "\\.(R|Rmd)$",   # matches .R or .Rmd
  full.names = TRUE,
  recursive = TRUE
)
# Texte à remplacer
old_text <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/Overleaf_elec"
new_text <- "Z:/Overleaf_elec"

# Remplacement dans tous les fichiers
for (f in files) {
  content <- readLines(f, warn = FALSE)
  content <- gsub(old_text, new_text, content, fixed = TRUE)
  writeLines(content, f)
}

