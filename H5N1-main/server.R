Sys.setenv(PATH = paste(Sys.getenv("PATH"), "/home/paul_dea/ncbi-blast-2.16.0+/bin", sep = ":"))
library(shiny)
library(reticulate)
library(R6)
library(rBLAST)
library(Biostrings)  # Pour DNAStringSet
library(msa)         # Pour les alignements multiples
library(ape)         # Pour l'arbre phylogénétique
library(phangorn)    # Pour les modèles évolutifs

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
    fasta_sequences <- read_fasta(input$fastaFile$datapath)
    
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
    
    if ("phylogeny" %in% input$analysisOptions) {
      output$phylogenyMessage <- renderText({
        paste("Calcul de l'arbre phylogénétique en cours...")
      })
    } else {
      output$phylogenyMessage <- renderText(NULL)
    }
    
    # Si l'option phylogénie est activée, procéder à la préparation des séquences
    if ("phylogeny" %in% input$analysisOptions) {
      
      # Chemin du dossier contenant les fichiers FASTA
      dossier <- "data"
      
      # Liste des fichiers FASTA dans le dossier
      fichiers_fasta <- list.files(dossier, pattern = "\\.fna$|\\.fa$|\\.fasta$", full.names = TRUE)
      if (length(fichiers_fasta) == 0) {
        showNotification("Aucun fichier FASTA trouvé dans le dossier 'data'.", type = "error")
        return(NULL)
      }
      
      # Charger toutes les séquences FASTA
      sequences <- lapply(fichiers_fasta, readDNAStringSet)
      combined_sequences <- do.call(c, sequences)
      
      # Si une séquence est chargée depuis un fichier (ou base de données)
      seq_obj <- sequence_obj()
      if (!is.null(seq_obj)) {
        # Convertir la séquence en format FASTA
        seq_fasta <- DNAStringSet(seq_obj$sequence)
        names(seq_fasta) <- seq_obj$id
        # Ajouter la séquence au jeu de données
        combined_sequences <- c(combined_sequences, seq_fasta)
      }
      showNotification("Séquence chargée ajoutée à la liste.", type = "message", duration = 3, id = "sequenceaj")
      
      # Démarrer l'analyse (en parallèle)
      showNotification("Création de l'arbre phylogénétique en cours, veuillez patienter...", type = "message", duration = NULL, id = "loading")
      
      # Réalisation de l'alignement des séquences
      alignment_phyDat <- phyDat(as.matrix(msa(combined_sequences, method = "ClustalOmega")), type = "DNA")
      
      # Création de l'arbre phylogénétique
      tree_start <- NJ(dist.ml(alignment_phyDat))
      tree_ml <- pml(tree_start, data = alignment_phyDat)
      tree_optim <- optim.pml(tree_ml, model = "GTR", optGamma = TRUE)
      
      # Enregistrer l'arbre optimisé dans le réactif
      phylo_tree(tree_optim)
      
      # Afficher l'arbre phylogénétique
      output$treePlot <- renderPlot({
        req(phylo_tree())  # Attendre que l'arbre soit disponible
        plot(phylo_tree()$tree, type = "phylogram", main = "Arbre phylogénétique par maximum de vraisemblance")
      })
      
      # Retirer la notification une fois l'analyse terminée
      removeNotification(id = "loading")
      
      # Permettre le téléchargement de l'arbre au format Newick
      output$downloadTree <- downloadHandler(
        filename = function() { paste("phylogenetic_tree_ml", Sys.Date(), ".nwk", sep = "") },
        content = function(file) {
          write.tree(phylo_tree()$tree, file = file)
        }
      )
    }
  })
}
