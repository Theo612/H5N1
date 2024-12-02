library(shiny)
library(shinycssloaders)  # Pour la barre de chargement
library(Biostrings)
library(msa)
library(ape)
library(phangorn)
library(phytools)

# Interface utilisateur
ui <- fluidPage(
  titlePanel("Analyse Phylogénétique"),
  sidebarLayout(
    sidebarPanel(
      actionButton("run_analysis", "Lancer l'analyse")
    ),
    mainPanel(
      # Ajouter un spinner autour du plot
      withSpinner(plotOutput("treePlot"), type = 4) ,
      downloadButton("downloadTree", "Télécharger l'arbre")
    )
  )
)

# Serveur
server <- function(input, output) {
  
  observeEvent(input$run_analysis, {
    # Chemin du dossier contenant les fichiers FASTA
    dossier <- "fasta"
    
    # Liste des fichiers FASTA dans le dossier
    fichiers_fasta <- list.files(dossier, pattern = "\\.fna$|\\.fa$|\\.fasta$", full.names = TRUE)
    
    # Vérifier qu'il y a des fichiers dans le dossier
    if (length(fichiers_fasta) == 0) {
      showNotification("Aucun fichier FASTA trouvé dans le dossier 'data'.", type = "error")
      return(NULL)
    }
    
    # Afficher une notification que l'analyse est en cours
    showNotification("Analyse en cours, veuillez patienter...", type = "message", duration = NULL, id = "loading")
    
    # Charger toutes les séquences FASTA
    sequences <- lapply(fichiers_fasta, readDNAStringSet)
    combined_sequences <- do.call(c, sequences)
    
    # Effectuer l'alignement des séquences
    alignment <- msa(combined_sequences, method = "ClustalOmega")
    
    # Conversion de l'alignement en un objet phyDat
    alignment_phyDat <- phyDat(as.matrix(alignment), type = "DNA")
    
    # Construire l'arbre de départ et l'arbre de maximum de vraisemblance
    tree_start <- NJ(dist.ml(alignment_phyDat))
    tree_ml <- pml(tree_start, data = alignment_phyDat)
    tree_optim <- optim.pml(tree_ml, model = "GTR", optGamma = TRUE)
    
    # Retirer la notification une fois l'analyse terminée
    removeNotification(id = "loading")
    
    # Afficher l'arbre
    output$treePlot <- renderPlot({
      plot(tree_optim$tree, type = "phylogram", main = "Arbre phylogénétique par maximum de vraisemblance")
    })
    
    # Télécharger l'arbre au format Newick
    output$downloadTree <- downloadHandler(
      filename = function() { paste("phylogenetic_tree_ml", Sys.Date(), ".nwk", sep = "") },
      content = function(file) {
        write.tree(tree_optim$tree, file = file)
      }
    )
  })
}

# Lancer l'application Shiny
shinyApp(ui = ui, server = server)
