# server.R
library(shiny)
library(reticulate)
library(jsonlite)
library(R6)

# Charger le script Python
source("utils.R")

#Création BDD
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
convert_folder_fasta_to_json("data", "data/h5n1_sequences.json")

Sequence <- R6::R6Class("Sequence",
                        public = list(
                          id = NULL,
                          sequence = NULL,
                          
                          # Constructeur de la classe
                          initialize = function(id, sequence) {
                            self$id <- id
                            self$sequence <- sequence
                          },
                          
                          # Méthode pour afficher la description de la séquence
                          print_info = function() {
                            return(sprintf("Sequence ID: %s, Length: %d", self$id, nchar(self$sequence)))
                          }
                        )
)

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
    
    # Créer un objet de la classe Sequence
    sequence_obj(Sequence$new(seq_id, seq))
    
    # Mettre à jour l'affichage
    output$globalAnalysis <- renderText({
      paste("Objet Sequence créé à partir du fichier chargé :",
            sprintf("ID: %s, Sequence: %s", seq_id, substr(seq, 1, 50)),
            "...")
    })
  })
  
  # Afficher l'information de la séquence (par exemple pour debug)
  output$debugOutput <- renderText({
    seq_obj <- sequence_obj()
    if (is.null(seq_obj)) {
      return("Aucune séquence chargée.")
    } else {
      return(seq_obj$print_info())  # Appeler la méthode print_info
    }
  })
  
  # Gestion de l'événement lorsqu'une séquence sauvegardée est sélectionnée et analysée
  observeEvent(input$analyze, {
    req(input$savedSequence != "") # Vérifier qu'une séquence sauvegardée est sélectionnée
    
    # Charger la séquence depuis le fichier sauvegardé
    fasta_sequences <- read_fasta(paste0("data/", input$savedSequence))
    
    # Utiliser uniquement la première séquence du fichier sélectionné
    seq_id <- names(fasta_sequences)[1]
    seq <- fasta_sequences[[1]]
    
    # Créer un objet de la classe Sequence
    sequence_obj(Sequence$new(seq_id, seq))
    
    # Mettre à jour l'affichage
    output$globalAnalysis <- renderText({
      paste("Objet Sequence créé à partir de la séquence sauvegardée :",
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
