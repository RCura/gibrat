library(shiny)

# Define server logic for random distribution application
shinyServer(function(input, output) {
    
    readData <- reactive({
        inFile <- input$csvInput
        
        if (is.null(inFile))
            return(NULL)
        
        return(read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote, check.names=FALSE))
        
    })
    
    computeGrowthTable <- reactive({
        if (!is.null(readData())) {
            compute_growthtable(readData())
        } else {
            return()
        }
        
    })
    
    output$data <- renderDataTable({
        if (!is.null(readData())){
            readData()
        } else {
            return()
        }
        
    })
    
    output$growthTable <- renderDataTable({
        if (!is.null(readData())){
            computeGrowthTable()
        } else {
            return()
        }
        
    })
    

})

