library(shiny)
library(shinydashboard)

html_dir <- file.path(getwd(), "GenomeBrowser_H5N1")
html_file <- file.path(html_dir, "index.html")

shiny::addResourcePath("genomeBrowser", html_dir)

ui <- dashboardPage(
  skin = "blue",
  
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
    actionButton("analyze", "Lancer l'analyse")
  ),
  
  #Contenu principal
  dashboardBody(
    #Style personnalisé pour occuper toute la hauteur
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          height: calc(100vh - 50px); /* Prend toute la hauteur de la fenêtre moins l'en-tête */
          overflow-y: hidden;
        }

        .tab-content {
          height: 100%;
          overflow-y: auto; 
        }

        iframe {
          height: calc(100vh - 120px); /* Ajustement pour l'en-tête et l'espacement */
          width: 100%;
          border: none; 
        }

        body {
          margin: 0;
          padding: 0;
          height: 100vh;
          overflow: hidden;
        }
      "))
    ),
    
    tabsetPanel(
      tabPanel("Analyse globale", verbatimTextOutput("globalAnalysis")),
      tabPanel("Analyse locale", verbatimTextOutput("localAnalysis")),
      tabPanel("Phylogénétique", plotOutput("phylogenyPlot")),
      tabPanel("Visualisation", 
               tags$iframe(src = "genomeBrowser/index.html", 
                           width = "100%", height = "100%"))
    )
  )
)

server <- function(input, output) {
  #Logique du serveur
}

shinyApp(ui = ui, server = server)


