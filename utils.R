# utils.R

# Fonction utilitaire pour lire un fichier FASTA
read_fasta <- function(file_path) {
  lines <- readLines(file_path)
  sequences <- list()
  current_id <- NULL
  current_seq <- ""
  
  for (line in lines) {
    if (startsWith(line, ">")) {
      # Enregistrer la séquence précédente
      if (!is.null(current_id)) {
        sequences[[current_id]] <- current_seq
      }
      # Extraire l'ID de la nouvelle séquence
      current_id <- substr(line, 2, nchar(line))
      current_seq <- ""
    } else {
      # Ajouter les lignes de séquence
      current_seq <- paste0(current_seq, line)
    }
  }
  
  # Ajouter la dernière séquence
  if (!is.null(current_id)) {
    sequences[[current_id]] <- current_seq
  }
  
  return(sequences)
}