library(shiny)
library(jsonlite)

#Fonction pour lire tous les fichiers FASTA d'un dossier et les convertir en un JSON unique
convert_folder_fasta_to_json <- function(folder_path, json_file) {
  #Initialiser la base de données JSON
  sequence_db <- list(sequences = list())

  #Lister tous les fichiers FASTA dans le dossier spécifié
  fasta_files <- list.files(folder_path, pattern = "\\.fasta$", full.names = TRUE)
  
  #Parcourir chaque fichier FASTA
  for (fasta_file in fasta_files) {
    fasta_lines <- readLines(fasta_file)
    sequences <- list()
    current_id <- NULL
    current_seq <- ""
    
    for (line in fasta_lines) {
      if (startsWith(line, ">")) {
        #Sauvegarder la séquence précédente
        if (!is.null(current_id)) {
          sequences[[current_id]] <- current_seq
        }
        #Extraire l'ID de séquence
        current_id <- substr(line, 2, nchar(line))
        current_seq <- ""
      } else {
        #Ajouter à la séquence courante
        current_seq <- paste0(current_seq, line)
      }
    }
    #Sauvegarder la dernière séquence
    if (!is.null(current_id)) {
      sequences[[current_id]] <- current_seq
    }

    #Ajouter les séquences de ce fichier au JSON principal
    sequence_db$sequences <- c(sequence_db$sequences, sequences)
  }

  #Sauvegarder la base de données consolidée en JSON
  write_json(sequence_db, json_file, pretty = TRUE)
}

#Exécution de la fonction pour le dossier data
convert_folder_fasta_to_json("data", "data/h5n1_sequences.json")

function(input, output, session) {

  observeEvent(input$fastaFile, {
    sequence <- readDNAStringSet(input$fastaFile$datapath)
    saveRDS(sequence, file = paste0("data/", input$fastaFile$name, ".fasta"))
  })
  
  #Charger la séquence sélectionnée pour comparaison
  selectedSequence <- reactive({
    req(input$savedSequence)
    readRDS(paste0("data/", input$savedSequence))
  })

}

