# server.R
library(shiny)
library(reticulate)
library(jsonlite)

#Charger le script Python et utils.R
source_python("sequence.py")
source("utils.R")

#Partie pour banque de données 
convert_folder_fasta_to_json <- function(folder_path, json_file) {
  # Initialiser la base de données JSON
  sequence_db <- list(sequences = list())
  
  # Lister tous les fichiers FASTA dans le dossier spécifié
  fasta_files <- list.files(folder_path, pattern = "\\.fasta$", full.names = TRUE)
  
  # Parcourir chaque fichier FASTA
  for (fasta_file in fasta_files) {
    fasta_lines <- readLines(fasta_file)
    sequences <- list()
    current_id <- NULL
    current_seq <- ""
    
    for (line in fasta_lines) {
      if (startsWith(line, ">")) {
        # Sauvegarder la séquence précédente
        if (!is.null(current_id)) {
          sequences[[current_id]] <- current_seq
        }
        # Extraire l'ID de séquence
        current_id <- substr(line, 2, nchar(line))
        current_seq <- ""
      } else {
        # Ajouter à la séquence courante
        current_seq <- paste0(current_seq, line)
      }
    }
    # Sauvegarder la dernière séquence
    if (!is.null(current_id)) {
      sequences[[current_id]] <- current_seq
    }
    
    # Ajouter les séquences de ce fichier au JSON principal
    sequence_db$sequences <- c(sequence_db$sequences, sequences)
  }
  
  # Sauvegarder la base de données consolidée en JSON
  write_json(sequence_db, json_file, pretty = TRUE)
}

#Exécution de la fonction pour le dossier data
convert_folder_fasta_to_json("data", "data/h5n1_sequences.json")



server <- function(input, output, session) {
  # Réactif pour stocker l'objet Sequence
  sequence_obj <- reactiveVal(NULL)
  
  # Gestion de l'événement lorsqu'un fichier est chargé
  observeEvent(input$fastaFile, {
    req(input$fastaFile)
    
    # Lire le fichier FASTA avec la fonction read_fasta
    fasta_sequences <- read_fasta(input$fastaFile$datapath)
    
    # Utiliser uniquement la première séquence pour cet exemple
    seq_id <- names(fasta_sequences)[1]
    seq <- fasta_sequences[[1]]
    
    # Créer un objet Python de la classe Sequence
    sequence_obj(Sequence(seq_id, seq))
    
    # Mettre à jour l'affichage
    output$globalAnalysis <- renderText({
      paste("Objet Python créé à partir du fichier chargé :",
            sprintf("ID: %s, Sequence: %s", seq_id, substr(seq, 1, 50)),
            "...")
    })
  })
  
  # Gestion de l'événement lorsqu'une séquence sauvegardée est sélectionnée et analysée
  observeEvent(input$analyze, {
    req(input$savedSequence != "") # Vérifier qu'une séquence sauvegardée est sélectionnée
    
    # Charger la séquence depuis le fichier sauvegardé
    fasta_sequences <- read_fasta(paste0("data/", input$savedSequence))
    
    # Utiliser uniquement la première séquence du fichier sélectionné
    seq_id <- names(fasta_sequences)[1]
    seq <- fasta_sequences[[1]]
    
    # Créer un objet Python de la classe Sequence
    sequence_obj(Sequence(seq_id, seq))
    
    # Mettre à jour l'affichage
    output$globalAnalysis <- renderText({
      paste("Objet Python créé à partir de la séquence sauvegardée :",
            sprintf("ID: %s, Sequence: %s", seq_id, substr(seq, 1, 50)),
            "...")
    })
  })
  
  # Gestion des séquences sauvegardées (optionnel)
  selectedSequence <- reactive({
    req(input$savedSequence)
    read_fasta(paste0("data/", input$savedSequence))
  })
}
