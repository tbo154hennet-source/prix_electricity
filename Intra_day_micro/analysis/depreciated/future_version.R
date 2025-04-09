long_format_function <- function(data) {
  counter <- 0  # Réinitialisation du compteur
  result_list <- list()  # Initialisation de la liste
  
  result_list <- lapply(seq_len(nrow(data)), function(i) {
    counter <<- counter + 1
    if (counter %% 200 == 0) message("Lignes traitées : ", counter)
    
    tryCatch({
      long_format(data[i, ], result_list)
    }, error = function(e) {
      print(data[i, "mrid"])  # Afficher l'identifiant problématique
      return(NULL)  # Ignorer les erreurs
    })
  })
  
  # Supprimer les NULL dans la liste et convertir en dataframe
  cleaned_list <- Filter(Negate(is.null), result_list)
  long_format_indispo <- bind_rows(cleaned_list)
  
  return(long_format_indispo)
}

# Conversion finale en dataframe
long_format_indispo <- bind_rows(non_duplicated_indispo)


