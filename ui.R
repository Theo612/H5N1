library(shiny)
library(shinydashboard)

html_dir <- file.path(getwd(), "GenomeBrowser_H5N1")
html_file <- file.path(html_dir, "index.html")

shiny::addResourcePath("genomeBrowser", html_dir)

ui <- dashboardPage(
  skin = "green",
  
  dashboardHeader(
    title = "Plateforme Génétique H5N1",
    titleWidth = 300
  ),
  
  #Menu lateral
  dashboardSidebar(
    width = 300,  #Largeur du sidebar
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
    actionButton("analyze", "Lancer l'analyse"),
    
    textOutput("debugOutput")
  ),
  
  #Contenu principal
  dashboardBody(
    tabsetPanel(
      tabPanel("Analyse globale", verbatimTextOutput("globalAnalysis")),
      tabPanel("Analyse locale", verbatimTextOutput("localAnalysis")),
      tabPanel("Phylogénétique", plotOutput("phylogenyPlot")),
      tabPanel("Visualisation", 
               tags$iframe(src = "genomeBrowser/index.html", 
                           width = "100%", height = "800px"))
    )
  )
)


