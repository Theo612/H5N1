library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  skin = "green",
  
  dashboardHeader(
    title = "Plateforme Génétique H5N1",
    titleWidth = 300
  ),
  
  dashboardSidebar(
    width = 300,  # Largeur du sidebar
    fileInput("fastaFile", "Charger une séquence FASTA", accept = ".fasta"),
    selectInput(
      "savedSequence", 
      "Sélectionner une séquence sauvegardée",
      choices = c("Aucune sélection" = "", list.files("data", pattern = ".fasta")), 
      selected = ""
    ),
    
    tags$div("Options d'analyse :", 
             style = "text-align: center; font-size: 16px; font-weight: bold;"),
    
    checkboxGroupInput("analysisOptions", "Choisissez les analyses à effectuer :",
                       choices = list(
                         "Alignement local" = "local",
                         "Alignement global" = "global",
                         "Phylogénétique" = "phylogeny"
                       )
    ),
    actionButton("run_analysis", "Lancer l'analyse"),
    
    uiOutput("debugOutput")
  ),
  
  dashboardBody(
    tabsetPanel(
      tabPanel("Analyse globale", verbatimTextOutput("globalAnalysis")),
      tabPanel("Analyse locale", verbatimTextOutput("localAnalysis")),
      tabPanel("Phylogénétique", 
               downloadButton("downloadTree", "Télécharger l'arbre"),
               downloadButton("downloadSequence", "Télécharger la séquence ajoutée"), 
               plotOutput("treePlot")),
      tabPanel("Visualisation", 
               tags$iframe(src = "genomeBrowser/index.html", 
                           width = "100%", height = "800px"))      
    )
  )
)