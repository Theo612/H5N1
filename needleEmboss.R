# needleEmboss.R

# Fonction pour exécuter EMBOSS needle
run_needle <- function(user_sequence,user_id, reference_file, output_file) {
  
  user_file <- tempfile(fileext = ".fasta")
  
  # Écrire la séquence utilisateur dans un fichier FASTA temporaire
  writeLines(c(paste(">", user_id), user_sequence), user_file)
  
  
  # Commande pour EMBOSS needle
  cmd <- paste(
    "needle",
    "-asequence", reference_file,
    "-bsequence", user_file,
    "-gapopen 10",    # Valeur par défaut de gap open
    "-gapextend 0.5", # Valeur par défaut de gap extend
    "-outfile", output_file
  )
  
  # Exécuter la commande dans le système
  system(cmd)
  
  # Lire les résultats de l'alignement si le fichier a été créé
  if (file.exists(output_file)) {
    results <- readLines(output_file)
    return(results)
  } else {
    stop("Erreur : Impossible de lire le fichier de sortie de l'alignement.")
  }
}

