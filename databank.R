library(jsonlite)

# Fonction pour lire les fichiers FASTA
read_fasta <- function(file_path) {
  lines <- readLines(file_path)
  sequences <- list()
  current_id <- NULL
  current_seq <- ""
  
  for (line in lines) {
    if (startsWith(line, ">")) {
      if (!is.null(current_id)) {
        sequences[[current_id]] <- current_seq
      }
      current_id <- substr(line, 2, nchar(line))
      current_seq <- ""
    } else {
      current_seq <- paste0(current_seq, line)
    }
  }
  if (!is.null(current_id)) {
    sequences[[current_id]] <- current_seq
  }
  
  return(sequences)
}

# Fonction pour convertir un dossier de FASTA en JSON
convert_folder_fasta_to_json <- function(folder_path, json_file) {
  sequence_db <- list(sequences = list())
  fasta_files <- list.files(folder_path, pattern = "\\.fasta$", full.names = TRUE)
  for (fasta_file in fasta_files) {
    fasta_lines <- readLines(fasta_file)
    sequences <- list()
    current_id <- NULL
    current_seq <- ""
    
    for (line in fasta_lines) {
      if (startsWith(line, ">")) {
        if (!is.null(current_id)) {
          sequences[[current_id]] <- current_seq
        }
        current_id <- substr(line, 2, nchar(line))
        current_seq <- ""
      } else {
        current_seq <- paste0(current_seq, line)
      }
    }
    if (!is.null(current_id)) {
      sequences[[current_id]] <- current_seq
    }
    sequence_db$sequences <- c(sequence_db$sequences, sequences)
  }
  write_json(sequence_db, json_file, pretty = TRUE)
}

# Exemple de création initiale de la base de données
convert_folder_fasta_to_json("data", "data/h5n1_sequences.json")
