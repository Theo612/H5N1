library(shiny)
library(reticulate)
library(R6)
library(rBLAST)
library(Biostrings)

source("utils.R")
source("databank.R")  
source("needleEmboss.R")
source("rblast_local.R")

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
                            return(sprintf("<b>Nom de la séquence chargée :</b> %s<br><b>Longueur :</b> %d", self$id, nchar(self$sequence)))
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
    fasta_sequence <- read_fasta(input$fastaFile$datapath)
    
    # Extraire l'identifiant et la séquence (un seul élément dans le fichier)
    seq_id <- names(fasta_sequences)[1]
    seq <- fasta_sequences[[seq_id]]
    
    # Créer un objet de la classe Sequence
    sequence_obj(Sequence$new(seq_id, seq))
  })
  
  # Afficher l'information de la séquence (par exemple pour debug)
  output$debugOutput <- renderUI({
    seq_obj <- sequence_obj()
    if (is.null(seq_obj)) {
      HTML("<div style='margin-left: 20px;'>Aucune séquence chargée.</div>")
    } else {
      HTML(sprintf("<div style='margin-left: 20px;'>%s</div>", seq_obj$print_info()))
    }
  })
  
  # Gestion de l'événement lorsqu'une séquence sauvegardée est sélectionnée et analysée
  observeEvent(input$analyze, {
    req(input$savedSequence != "") # Vérifier qu'une séquence sauvegardée est sélectionnée
    
    # Charger la séquence depuis le fichier sauvegardé
    fasta_sequences <- read_fasta(paste0("data/", input$savedSequence))
    
    # Extraire l'identifiant et la séquence (un seul élément dans le fichier)
    seq_id <- names(fasta_sequences)[1]
    seq <- fasta_sequences[[seq_id]]
    
    # Créer un objet de la classe Sequence
    sequence_obj(Sequence$new(seq_id, seq))
  })
  
  observeEvent(input$analyze, {

    bdd_rblastlocal <- file.path(getwd(), "data/bdd_rblastl_local/database") # Le chemin de la base de données
    res_rblast <- run_rblast(input$fastaFile$datapath, bdd_rblastlocal) # Appel de la fonction d'alignement local et stockage des résultats
    
    req(sequence_obj())  # S'assurer qu'une séquence est disponible
    
    reference_sequence <- "data/sequenceH5N1.fasta"
    
    # Appel à la fonction run_needle pour faire l'alignement
    output_file <- tempfile(fileext = ".needle")  # Fichier de sortie temporaire
    
    result <- run_needle(sequence_obj()$sequence,sequence_obj()$id, reference_sequence, output_file)
    
    output$globalAnalysis <- renderText({
      if ("global" %in% input$analysisOptions) {
        paste("Résultats de l'alignement global :\n", paste(result, collapse = "\n"))
      }
    })
    
    output$localAnalysis <- renderText({
      if ("local" %in% input$analysisOptions) {
        if (is.null(res_rblast)) {
          return("Aucun résultat trouvé.")
        }
        
        # Transformer les résultats en table
        results_df <- as.data.frame(res_rblast)
        colnames(results_df) <- c("Query ID", "Subject ID", "% Identity", "Alignment Length", 
                                  "Mismatches", "Gap Opens", "Query Start", "Query End", 
                                  "Subject Start", "Subject End", "E-value", "Bit Score")
        
        # Convertir la table en texte
        results_text <- apply(results_df, 1, function(row) {
          paste(sprintf("Query: %s | Subject: %s | %% Identity: %.2f%% | Length: %d | E-value: %s",
                        row["Query ID"], row["Subject ID"], as.numeric(row["% Identity"]), 
                        as.integer(row["Alignment Length"]), row["E-value"]))
        })
        
        # Afficher les résultats
        paste("Résultats de l'alignement local :\n", paste(results_text, collapse = "\n"))
      }
    })
    
    output$phylogenyPlot <- renderPlot({
      if ("phylogeny" %in% input$analysisOptions) {
        plot(1:10, 1:10, main = "Arbre phylogénétique")
      }
    })
  })
}
