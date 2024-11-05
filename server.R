library(shiny)

# Define server logic required to draw a histogram
function(input, output, session) {

  observeEvent(input$fastaFile, {
    sequence <- readDNAStringSet(input$fastaFile$datapath)
    saveRDS(sequence, file = paste0("data/", input$fastaFile$name, ".rds"))
  })
  
  # Charger la séquence sélectionnée pour comparaison
  selectedSequence <- reactive({
    req(input$savedSequence)
    readRDS(paste0("data/", input$savedSequence))
  })

}
