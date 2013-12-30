require(shiny)

shinyUI(pageWithSidebar(
    #headerPanel("Gibrat Simulator"),
    headerPanel("Gibrat Simulator",
                
                tags$head(
                    conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                     tags$div(style="float:right; padding-right:30px; padding-top:10px;",
                                              tags$img(src="loading-gif-animation.gif",height=100,width=100)),
                                     tags$div(style="float:right; padding-right:30px; padding-top:10px; color:black;
                                          background-color:white; font-family:arial; font-size:18 px",
                                              "Calculating... Please wait...")
                    )
                )
    ),
    sidebarPanel(
        checkboxInput("csvSettings", "CSV Options", FALSE),
        conditionalPanel(
            condition="input.csvSettings == true",
            checkboxInput('header', 'Header', TRUE),
            radioButtons('sep', 'Separator',
                     c(Comma=',',
                       Semicolon=';',
                       Tab='\t'),
                     'Comma'),
            radioButtons('quote', 'Quote',
                     c(None='',
                       'Double Quote'='"',
                       'Single Quote'="'"),
                     'Double Quote')
            ),
        fileInput('csvInput', 'Choose CSV File',
                  accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
        actionButton(inputId="testData", label="load test dataset"),
        tags$hr(),
        numericInput(inputId="nbReplications", label="Number of replications", value=30, min=10, max=200, step=10 ),
        tags$hr(),
        actionButton(inputId="runSim", label="Run simulation"),
        conditionalPanel(condition="$('html').hasClass('shiny-busy')", h5("Wait..."))
    ),
    mainPanel(
    tabsetPanel(
        tabPanel("Base data", dataTableOutput('data')),
        tabPanel("Growth", 
                 plotOutput("growthPlot"),
                 dataTableOutput('growthTable')),
        tabPanel("Plots",
                 h4("Computation can be long, be patient..."),
                 plotOutput('gibratRankSize')),
        tabPanel("Correlations", htmlOutput('correlationsFigures')),
        tabPanel("About", includeMarkdown(path="README.md"))
        
    )
    )
))
