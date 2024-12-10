library(shiny)
library(reticulate)
library(R6)
library(Biostrings)  # Pour DNAStringSet
library(msa)         # Pour les alignements multiples
library(ape)         # Pour l'arbre phylogénétique
library(phangorn)    # Pour les modèles évolutifs
library(seqinr)

source("utils.R")

# Définition de la classe Sequence
Sequence <- R6::R6Class("Sequence",
                        public = list(
                          id = NULL,
                          sequence = NULL,
                          
                          initialize = function(id, sequence) {
                            self$id <- id
                            self$sequence <- sequence
                          },
                          
                          print_info = function() {
                            sprintf("<b>Nom de la séquence chargée :</b> %s<br><b>Longueur :</b> %d", self$id, nchar(self$sequence))
                          }
                        ))

# Serveur Shiny
server <- function(input, output, session) {
  
  # Réactif pour stocker l'objet Sequence
  sequence_obj <- reactiveVal(NULL)
  
  # Réactif pour stocker l'arbre phylogénétique
  phylo_tree <- reactiveVal(NULL)
  
  # Charger une séquence depuis un fichier FASTA
  observeEvent(input$fastaFile, {
    req(input$fastaFile)
    fasta_sequences <- read_fasta(input$fastaFile$datapath)
    
    # Extraire la première séquence
    seq_id <- names(fasta_sequences)[1]
    seq <- fasta_sequences[[seq_id]]
    
    # Créer un objet Sequence
    sequence_obj(Sequence$new(seq_id, seq))
  })
  
  # Charger une séquence sauvegardée
  observeEvent(input$savedSequence, {
    req(input$savedSequence != "")
    fasta_sequences <- read_fasta(paste0("data/", input$savedSequence))
    
    # Extraire la première séquence
    seq_id <- names(fasta_sequences)[1]
    seq <- fasta_sequences[[seq_id]]
    
    # Créer un objet Sequence
    sequence_obj(Sequence$new(seq_id, seq))
  })
  
  # Afficher les informations de la séquence
  output$debugOutput <- renderUI({
    seq_obj <- sequence_obj()
    if (is.null(seq_obj)) {
      HTML("<div style='margin-left: 20px;'>Aucune séquence chargée.</div>")
    } else {
      HTML(sprintf("<div style='margin-left: 20px;'>%s</div>", seq_obj$print_info()))
    }
  })
  
  # Lancer l'analyse dès que le bouton est cliqué
  observeEvent(input$run_analysis, {
    
    # Afficher immédiatement les messages pour les analyses sélectionnées
    if ("global" %in% input$analysisOptions) {
      output$globalAnalysis <- renderText({
        paste("Résultats de l'alignement global...")
      })
    } else {
      output$globalAnalysis <- renderText(NULL)
    }
    
    if ("local" %in% input$analysisOptions) {
      output$localAnalysis <- renderText({
        paste("Résultats de l'alignement local...")
      })
    } else {
      output$localAnalysis <- renderText(NULL)
    }
    
    # Afficher le message pour l'analyse phylogénétique si sélectionnée
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
