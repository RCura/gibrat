require(shiny)

shinyUI(pageWithSidebar(
    #headerPanel("Gibrat Simulator"),
    headerPanel("Gibrat Simulator"),
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
        actionButton(inputId="testData", label="Load test dataset"),
        tags$hr(),
        selectInput("idColumn", "ID", choices="", multiple=FALSE),
        selectInput("timeColumnSelected", "Time columns :",choices="", multiple=TRUE)
       
    ),
    mainPanel(
    tabsetPanel(
        tabPanel("Base data", dataTableOutput('data')),
        tabPanel("Growth",
                 tags$h4("Observed"),
                 dataTableOutput('growthTable'),
                 tags$h4('Average yearly growth rate'),
                 plotOutput("growthPlot"),
                 downloadButton(outputId="dlButton", label="Download table")),
        tabPanel("Simulation",
                 numericInput(inputId="nbReplications", label="Number of replications", value=10, min=5, max=100, step=5 ),
                 tags$hr(),
                 actionButton(inputId="runSim", label="Run simulation"),
                 conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                  tags$div(style="float:left;",
                                           tags$img(src="loading-gif-animation.gif",height=50,width=50)),
                                  h4("Computation can be long, be patient...")),
                 plotOutput('gibratRankSize')),
        tabPanel("Correlations", tableOutput('correlations')),
        tabPanel("About", includeMarkdown(path="README.md"))
        
    )
    )
))
