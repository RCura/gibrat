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
        numericInput(inputId="nbReplications", label="Number of replications", value=30)
    ),
    mainPanel(
    tabsetPanel(
        tabPanel("Base data", dataTableOutput('data')),
        tabPanel("Plots"),
        tabPanel("Growth Table", dataTableOutput('growthTable')),
        tabPanel("Correlations"),
        tabPanel("About")
        
    )
    )
))
