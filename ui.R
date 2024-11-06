library(shiny)

ui <- fluidPage(
  titlePanel(
    h1("Plateforme Génétique H5N1", align="center"),
    ),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("fastaFile", "Charger une séquence FASTA", accept = ".fasta"),
      selectInput("savedSequence", "Sélectionner une séquence sauvegardée",
                  choices = list.files("data", pattern = "\\.rds$")),
      actionButton("analyze", "Lancer l'analyse")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Analyse globale", verbatimTextOutput("globalAnalysis")),
        tabPanel("Analyse locale", verbatimTextOutput("localAnalysis")),
        tabPanel("Phylogénétique", plotOutput("phylogenyPlot")),
        tabPanel("Visualisation", htmlOutput("genomeBrowser"))
      )
    )
  )
)

server <- function(input, output) {
  # À compléter dans chaque section
}

shinyApp(ui = ui, server = server)

