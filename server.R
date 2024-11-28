# server.R
library(shiny)
library(reticulate)

# Charger le script Python
source_python("sequence.py")

# Charger le fichier utils.R
source("utils.R")

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