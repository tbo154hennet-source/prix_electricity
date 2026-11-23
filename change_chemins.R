# Dossier où sont tes scripts
folder <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab"
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
new_text <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab"

files <- c("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/depreciated/VAR_analysis_fr/supply_demand/irf_plot_esp_1var_bis.Rmd" )

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
new_text <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/Overleaf_elec"

# Remplacement dans tous les fichiers
for (f in files) {
  content <- readLines(f, warn = FALSE)
  content <- gsub(old_text, new_text, content, fixed = TRUE)
  writeLines(content, f)
}


source_path <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/Overleaf_elec/images/latex_tables"
setwd(source_path)

# Lister tous les fichiers .tex
tex_files <- list.files(pattern = "\\.tex$")

for (file in tex_files) {
  tex_lines <- readLines(file)
  
  # Vérifier si le fichier contient un tabular
  begin_idx <- grep("\\\\begin\\{tabular", tex_lines)
  end_idx   <- grep("\\\\end\\{tabular\\*?\\}", tex_lines)
  
  if (length(begin_idx) > 0 & length(end_idx) > 0) {
    # Retenir seulement les lignes entre begin et end tabular
    tabular_lines <- tex_lines[begin_idx:end_idx]
    
    # Réécrire dans le même fichier (ou changer le nom si tu veux garder l'original)
    writeLines(tabular_lines, file)
  }
}


