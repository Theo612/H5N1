# server.R
library(shiny)
library(reticulate)

# Charger le script Python
source_python("sequence.py")

# Charger le fichier utils.R
source("utils.R")

# Fichier pour sauvegarder toutes les séquences
all_sequences_file <- "all_sequences.txt"

server <- function(input, output, session) {
  # Réactif pour stocker l'objet Sequence
  sequence_obj <- reactiveVal(NULL)
  
  # Fonction pour écrire les séquences dans un fichier texte
  append_to_file <- function(file, seq_id, seq) {
    line <- sprintf("> %s\n%s\n", seq_id, seq)
    write(line, file = file, append = TRUE)
  }
  
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
    
    # Ajouter la séquence au fichier texte
    append_to_file(all_sequences_file, seq_id, seq)
    
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
    
    # Charger les séquences depuis le fichier sauvegardé
    fasta_sequences <- read_fasta(paste0("data/", input$savedSequence))
    
    # Ajouter toutes les séquences de ce fichier au fichier texte
    for (seq_id in names(fasta_sequences)) {
      seq <- fasta_sequences[[seq_id]]
      append_to_file(all_sequences_file, seq_id, seq)
    }
    
    # Utiliser la première séquence pour l'affichage
    first_seq_id <- names(fasta_sequences)[1]
    first_seq <- fasta_sequences[[first_seq_id]]
    
    # Créer un objet Python de la classe Sequence
    sequence_obj(Sequence(first_seq_id, first_seq))
    
    # Mettre à jour l'affichage
    output$globalAnalysis <- renderText({
      paste("Objet Python créé à partir de la séquence sauvegardée :",
            sprintf("ID: %s, Sequence: %s", first_seq_id, substr(first_seq, 1, 50)),
            "...")
    })
  })
  
  # Gestion des séquences sauvegardées (optionnel)
  selectedSequence <- reactive({
    req(input$savedSequence)
    read_fasta(paste0("data/", input$savedSequence))
  })
}
