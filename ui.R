require(shiny)

shinyUI(pageWithSidebar(
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
        tags$hr(),
        numericInput(inputId="nbReplications", label="Number of replications", value=30),
        conditionalPanel(condition="$('html').hasClass('shiny-busy')", h5("Wait..."))
    ),
    mainPanel(
    tabsetPanel(
        tabPanel("Base data", dataTableOutput('data')),
        tabPanel("Plots",
                 h4("Computation can be long, be patient..."),
                 plotOutput('gibratRankSize')),
        tabPanel("Growth Table", dataTableOutput('growthTable')),
        tabPanel("Correlations", htmlOutput('correlationsFigures')),
        tabPanel("About")
        
    )
    )
))
