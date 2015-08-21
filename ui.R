library(shiny)

shinyUI(
    
    navbarPage("Zipf",
               header = tags$head(includeScript("www/analytics.js")),
               position = "static-top",
               inverse = TRUE,
               tabPanel("Urban system scale",
                        sidebarLayout(
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
                                    tabPanel("LogNormality", 
                                             selectInput("dateLogNormal", "Date", choices="", multiple=FALSE,  selectize = TRUE),
                                             fluidRow(
                                                 column(6, plotOutput('plotLognormal')),
                                                 column(6,  plotOutput('qqplotLognormal'))
                                             ),
                                             DT::dataTableOutput('logNormalSummary'),
                                             h3("Parameters of a fitted lognormal distribution"),
                                             actionButton("runLogNormal", "Compute LogNormal parameters"),
                                             tableOutput('estimLognormal'),
                                             h4("Computation can be long, be patient..."),
                                             HTML("<h4><em>Note that depending on the database, it could also break the application</em></h4>"),
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
                                    #             tabPanel("debug gibrat",
                                    #                      DT::dataTableOutput('rawGrowthTable'),
                                    #                      DT::dataTableOutput('rawGrowthTableSizeInit')),
                                    tabPanel("Gibrat Correlations",
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
                                             fluidRow(
                                                 column(6, plotOutput('gibratRankSize')),
                                                 column(6, plotOutput('gibratExpectation'))
                                             ),
                                             fluidRow(
                                                 column(6, plotOutput('meanEvolution')),
                                                 column(6, plotOutput('sdEvolution'))
                                             ),
                                             downloadButton(outputId="simresultDL", label="Download simulations results")),
                                    #tabPanel("Correlations", tableOutput('correlations')),
                                    tabPanel("About", includeMarkdown(path="README.md"))
                                    
                                )
                            ) 
                        )    
               ),
               tabPanel("System comparison",
                        HTML("<h2>Here be <s>dragons</s> <i>tabs</i> & <i>plots</i></h2>")
                        )
               
    )
)
