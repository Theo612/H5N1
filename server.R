library(shiny)
library(reticulate)
library(R6)

source("utils.R")
source("databank.R")  #Importation des fonctions de gestion de la banque de données

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
  observeEvent(input$analyze, {
    req(input$fastaFile)
    
    # Lire le fichier FASTA avec la fonction read_fasta
    fasta_sequences <- read_fasta(input$fastaFile$datapath)
    
    # Utiliser uniquement la première séquence pour cet exemple
    seq_id <- names(fasta_sequences)[1]
    seq <- fasta_sequences[[1]]
    
    # Créer un objet de la classe Sequence
    sequence_obj(Sequence$new(seq_id, seq))
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
  })
  
  observeEvent(input$analyze, {
    output$globalAnalysis <- renderText({
      if ("global" %in% input$analysisOptions) {
        paste("Résultats de l'alignement global...")
      }
    })
    
    output$localAnalysis <- renderText({
      if ("local" %in% input$analysisOptions) {
        paste("Résultats de l'alignement local...")
      }
    })
    
    output$phylogenyPlot <- renderPlot({
      if ("phylogeny" %in% input$analysisOptions) {
        plot(1:10, 1:10, main = "Arbre phylogénétique")
      }
    })
  })
}
