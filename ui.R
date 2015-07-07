library(shiny)

shinyUI(pageWithSidebar(
    headerPanel("Gibrat Simulator", tags$head(includeScript("www/analytics.js"))),
    sidebarPanel(
        selectInput(inputId = 'dataset', label = "Choose country",
                    multiple = FALSE,
                    choices = c("South Africa", "Brazil", "Russia", "India", "China", "USA", "France")),
        tags$hr(),
        selectInput("timeColumnSelected", "Time columns :",choices="", multiple=TRUE, selectize=TRUE)
        
    ),
    mainPanel(
        tabsetPanel(
            tabPanel("Base data", DT::dataTableOutput('data')),
            tabPanel("Top 10", DT::dataTableOutput('top10')),
            tabPanel("City Size Classes", 
                     selectInput("dateClasses", "Date", choices="", multiple=FALSE,  selectize = TRUE), 
                     DT::dataTableOutput('sizeClasses')),
            tabPanel("Zipf", 
                     selectInput("dateZipf", "Date", choices="", multiple=FALSE,  selectize = TRUE),
                     plotOutput('plotZipf'),
                     h3("Parameters of a fitted power-law distribution"),
                     tableOutput('estimZipf')),
            tabPanel("Zipf Evolution",
                    plotOutput("zipfEvolution"),
                    DT::dataTableOutput("zipfEvolutionSummary")),
            tabPanel("LogNormal", 
                     selectInput("dateLogNormal", "Date", choices="", multiple=FALSE,  selectize = TRUE),
                     plotOutput('plotLognormal'),
                     h3("Parameters of a fitted lognormal distribution"),
                     actionButton("runLogNormal", "Compute LogNormal parameters"),
                     tableOutput('estimLognormal'),
                     h4("Computation can be long, be patient..."),
                     "cf. Gillespie C. S., 2015, Fitting heavy tailed distributions : 
                 the poweRlaw package, Journal of Statistical Software, Vol. 64, Issue 2."),
            tabPanel("Transition Matrices", 
                     fluidRow(
                         column(6,
                                selectInput("dateInitial", "Initial Date", choices="", multiple=FALSE,  selectize = TRUE)
                         ),
                         column(6,
                                selectInput("dateFinal", "Final Date", choices="", multiple=FALSE,  selectize = TRUE)
                         )
                     ),
                     fluidRow(
                         tableOutput('transitionMatrix')),
                     fluidRow(
                         tableOutput('transitionMatrixRel'))),
            tabPanel("Growth",
                     tags$h4("Observed"),
                     DT::dataTableOutput('growthTable'),
                     tags$h4('Average yearly growth rate'),
                     plotOutput("growthPlot"),
                     downloadButton(outputId="dlButton", label="Download table")),
            tabPanel("Gibrat Correlations",
                     #DT::dataTableOutput('rawGrowthTable'),
                     #DT::dataTableOutput('rawGrowthTableSizeInit'),
                     tags$h3('Growth rate and initial size correlation'),
                     DT::dataTableOutput('correlSizeGrowth'),
                     plotOutput('correlSizeGrowthPlot'),
                     tags$h3('Temporal auto-correlation'),
                     DT::dataTableOutput('correlTemporal'),
                     plotOutput('correlTemporalPlot')
                     ),
            tabPanel("Gibrat Simulation",
                     numericInput(inputId="nbReplications", label="Number of replications", value=10, min=5, max=100, step=5 ),
                     tags$hr(),
                     actionButton(inputId="runSim", label="Run simulation"),
                     conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                      tags$div(style="float:left;",
                                               tags$img(src="loading-gif-animation.gif",height=50,width=50)),
                                      h4("Computation can be long, be patient...")),
                     plotOutput('gibratRankSize'),
                     downloadButton(outputId="simresultDL", label="Download simulations results")),
            tabPanel("Correlations", tableOutput('correlations')),
            tabPanel("About", includeMarkdown(path="README.md"))
            
        )
    )
))
