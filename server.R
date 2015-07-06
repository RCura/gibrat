library(shiny)
library(ggplot2)
library(MASS)
library(poweRlaw)
library(DT)
# TODO : Add a computation of correlation for each census date observed/ mean of simulated

# Define server logic for random distribution application
shinyServer(function(input, output, session) {
    #options(warn=0, error=browser, shiny.error=browser)
    load("data/countriesPop.RData")
    
    dataValues <- reactiveValues(rawDF = NULL,
                             filtredDF = NULL,
                             calcDF = NULL,
                             growthTable = NULL,
                             lastCensusesTable  = NULL)
    
    computedValues <- reactiveValues(simData = NULL,
                                     simResults = NULL,
                                     simMeans = NULL,
                                     simSDs = NULL,
                                     meanRanks = NULL,
                                     simRanks = NULL,
                                     obsRanks = NULL)
    
    analysisValues <- reactiveValues(zipfTable = NULL,
                                     transitionMatrix = NULL,
                                     logNormalTable = NULL,
                                     zipfResTable = NULL
                                     )
    

    
    censusDate <- reactiveValues(datecol= NULL)
    observe({
      if (input$date == "Last Census") censusDate$datecol <- 4
      if (input$date == "Last Census - 1") censusDate$datecol <- 3
      if (input$date == "Last Census - 2") censusDate$datecol <- 2
      
    })
    
    observe({
        countryName <- input$dataset
        if (countryName ==  "Brazil") {
            dataValues$rawDF <- Brazil
        } else if (countryName ==  "Russia") {
            dataValues$rawDF <- Russia
        } else  if (countryName ==  "India"){
            dataValues$rawDF <- India
        }  else if (countryName ==  "China"){
            dataValues$rawDF <- China
        } else  if (countryName ==  "South Africa"){
            dataValues$rawDF <- SouthAfrica
        } else if (countryName ==  "USA"){
            dataValues$rawDF <- USA
        } else  {
            dataValues$rawDF <- France 
        }

        timeColumns <- as.numeric(na.omit(as.numeric(unlist(colnames(dataValues$rawDF)))))
        allColumns <- c("None", unlist(colnames(dataValues$rawDF)))
        updateInputs(session, allColumns, timeColumns)
        
    })
        
    observe({
        timeColumns <- input$timeColumnSelected
        if (!is.null(timeColumns) && timeColumns %in% names(dataValues$rawDF)){
            calcData <- dataValues$rawDF[timeColumns]
            row.names(calcData) <- dataValues$rawDF$ID
            dataValues$calcDF <- calcData
        }
    })
    
    observe({
        if (!is.null(dataValues$calcDF)) {
            dataValues$growthTable <- compute_yearly_growth_table(dataValues$calcDF)
        }
    })
    
    observe({
        if (!is.null(dataValues$calcDF)) {
            df <- dataValues$rawDF
            shortDF <- df[,c(1:2,(ncol(df) - 2):ncol(df))]
            dataValues$lastCensusesTable <- shortDF[order(shortDF[,-ncol(shortDF)]),]
        }
    })
    
#     observe({
#         if (!is.null(dataValues$lastCensusesTable)) {
#             dfZipf <- dataValues$lastCensusesTable
#             dfZipf <- dfZipf[dfZipf[,censusDate$datecol] > 0, censusDate$datecol]
#             dfZipf <- dfZipf[order(-dfZipf[,1]), ]
#             ranks <- 1:nrow(dfZipf)
#             zipf <- data.frame(ranks, dfZipf, stringsAsFactors =  FALSE, check.names = FALSE)
#             colnames(zipf) <- c("ranks", "size")
#             zipf$dates <- names(dataValues$lastCensusTable)[censusDate$datecol]
#             analysisValues$zipfTable <- zipf
#         }
#     })

exportTransitionMatrix <- reactive ({
  
  if (!is.null(dataValues$lastCensusesTable)) {
    df <- dataValues$lastCensusesTable
      
    if (input$datefinal == "Last Census") final <- 4
    if (input$datefinal == "Last Census - 1") final <- 3
    if (input$datefinal == "Last Census - 2") final <- 2
    
    if (input$dateinitial == "Last Census") initial <- 4
    if (input$dateinitial == "Last Census - 1") initial <- 3
    if (input$dateinitial == "Last Census - 2") initial <- 2
    
    valBreaks <- c(0, 10E3, 50E3, 100E3, 1E6, 10E6, 10E9)
    
    FinalPops <- df[,final]
    InitialPops <- df[,initial]
    FinalDate <- cut(x=FinalPops,breaks=valBreaks, include.lowest = TRUE, right = FALSE,
                     labels = c("< 10k", "10k - 50k",
                               "50k - 100k", "100k - 1M",
                               "1M - 10M",  "> 10M")) 
    InitialDate <- cut(x=InitialPops,breaks=valBreaks, include.lowest = TRUE, right = FALSE,
                       labels = c("< 10k", "10k - 50k",
                                 "50k - 100k", "100k - 1M",
                                 "1M - 10M",  "> 10M")) 
      
    transitionMatrix <- table(InitialDate,FinalDate)
    
    return(transitionMatrix)
  } else {
    return()
  }
  
})


exportLogNormalTable <- reactive({
  if (!is.null(dataValues$lastCensusesTable)) {
    df <- dataValues$lastCensusesTable
    dfLG <- df
    
    datecol <- censusDate$datecol
    
    dfLG$datepop <- dfLG[,datecol]
    dfLG <- subset(dfLG, datepop > 0)
    
    return(dfLG)
  } else {
    return()
  }
  
})
    
exportZipfResTable <- reactive({
  if (!is.null(analysisValues$zipfTable)) {
zipf <- analysisValues$zipfTable  
zipfCut10 <- subset(zipf, size >= 10000)
zipfCut100 <- subset(zipf, size >= 100000)      

model10 <- lm(log(size) ~ log(ranks), data=zipfCut10, na.action=na.omit)
ConfInt_model10 <- confint(model10)
model100 <- lm(log(size) ~ log(ranks), data=zipfCut100, na.action=na.omit)
ConfInt_model100 <- confint(model100)
res10 = data.frame(model10$coefficients[[2]], ConfInt_model10[2,1], ConfInt_model10[2,2], summary(model10)$r.squared)
colnames(res10) <- c("Estimated Zipf Exponent", "Lower bound", "Upper bound", "R squared")
res100 = data.frame(model100$coefficients[[2]], ConfInt_model100[2,1], ConfInt_model100[2,2], summary(model100)$r.squared)
colnames(res100) <- c("Estimated Zipf Exponent", "Lower bound", "Upper bound", "R squared")
res = rbind(res10, res100)
Estimation<- c("Population > 10000 hab.", "Population > 100000 hab.")
res <- cbind(Estimation, res)
return(res)
} else {
  return()
}
})


    simulationsData <- reactive({
        if (!is.null(dataValues$calcDF) && input$runSim > 0) {
            df <- dataValues$calcDF
            nbReps <- isolate(input$nbReplications)
            simData <- run_simulation(df=df, reps=nbReps)
            return(simData)
        } else {
            return()
        }
    })
    
    simResults <- reactive({
        if (!is.null(simulationsData())){
            simResults <- simulationsData()[,ncol(dataValues$calcDF),]
            return(simResults)
        } else {
            return()
        }
    })
    
    simMeans <- reactive({
        if (!is.null(simulationsData())){
            simMeans <- apply(X=simulationsData()[,,], 1:2, function(x){mean(x, na.rm=TRUE)})
            return(simMeans)
        } else {
            return()
        }
    })
    
    simSDs <- reactive({
        if (!is.null(simulationsData())){
            simSDs <- apply(X=simulationsData()[,,], 1:2, function(x){return(sd(x, na.rm=TRUE))})
            return(simSDs)
        } else {
            return()
        }
    })
    
    meanRanks <- reactive({
        if (!is.null(simMeans())){
            meanRanks <- create_rank_tables(obsdata=dataValues$calcDF, simMean=simMeans())
            return(meanRanks)
        } else {
            return()
        }
    })
    
    simRanks <- reactive({
        if (!is.null(meanRanks())) {
            simRank <- as.data.frame(meanRanks()[1])
            colnames(simRank) <- colnames(dataValues$calcDF)
            return(simRank)
        } else {
            return()
        }
    })
    
    obsRanks <- reactive({
        if (!is.null(meanRanks())) {
            obsRank <- as.data.frame(meanRanks()[2])
            colnames(obsRank) <- colnames(dataValues$calcDF)
            return(obsRank)
        } else {
            return()
        }
    })
    
    
    output$data <- renderDataTable({
        if (!is.null(dataValues$rawDF)) {
            dataValues$rawDF
        }
    }, rownames  = FALSE,
    filter = "bottom",
    extensions = list(FixedColumns = list(leftColumns = 2)),
    options = list(
        scrollX = TRUE,
        scrollCollapse = TRUE
    ),
    class = 'cell-border stripe')
    
    
    output$growthPlot <- renderPlot({
        if (!is.null(dataValues$calcDF)){
            growthTable <- dataValues$growthTable
            meanGrowth <- unlist(growthTable[1,])
            sdGrowth <- unlist(growthTable[2,])
            minY <- min(meanGrowth - sdGrowth) 
            maxY <- max(meanGrowth + sdGrowth)
            xLabels <- colnames(dataValues$calcDF)[-1]
            polyX <- c(xLabels, rev(xLabels))
            polyY <- c((meanGrowth + sdGrowth), rev(meanGrowth - sdGrowth))
            plot(y=unlist(growthTable[1,]), x=colnames(dataValues$calcDF)[-1],
                 ylim=c(minY, maxY), type="b", pch=4,
                 xlab="Year",
                 ylab="Growth (%)")
            polygon(polyX, polyY, col="blue", border = NA,density=50)
            
        }
        
    })
    
    exportGrowthTable <- reactive({
        if (!is.null(dataValues$calcDF)){
            growthTable <- dataValues$growthTable
            periodNames <- colnames(growthTable)
            indicatorNames <- row.names(growthTable)
            tGrowthTable <- cbind.data.frame(periodNames,t(growthTable))
            colnames(tGrowthTable) <- c("Period", indicatorNames)
            #View(tGrowthTable)
            return(tGrowthTable)
        } else {
            return()
        }
    })
    
    output$growthTable <- renderDataTable({
        exportGrowthTable()
    }, options = list(length = 50, dom = "t"))
    

    
    output$dlButton <- downloadHandler(
        filename = function() { paste(input$dataset, "_growth", '.csv', sep='') },
        content = function(file) {
            write.table(x=exportGrowthTable(), file=file,sep=",", dec=".", row.names=FALSE, col.names=TRUE, quote=TRUE)
        }
    )
    
    output$simresultDL <- downloadHandler(
        filename = function() {paste(input$dataset, "_simresults", ".csv", sep="")},
        content = function(file){
            exportDF <- simulationsData()[,ncol(dataValues$calcDF),]
            exportDF <- data.frame(ID=row.names(exportDF), exportDF)
            write.table(x=exportDF, file=file, sep=",", row.names=FALSE, col.names=TRUE, quote=TRUE)
        }
        )
    
    output$gibratRankSize <- renderPlot({
        if (is.null(simMeans())){ return()}
        
        lastTime <- ncol(dataValues$calcDF)
        
        cData <- na.omit(dataValues$calcDF[,lastTime])
        sData <- na.omit(simulationsData()[,lastTime,])
        mData <- na.omit(simMeans()[,lastTime])
        
        maxpop <- max(max(cData),max(sData))
        minpop <- min(min(cData),min(sData))
        
        plot(x=sort(rank(-cData), decreasing=T), y=sort(cData, decreasing=F), log="xy", type="l", ylim=c(minpop,maxpop), xlab="Rang", ylab="Population", col="darkblue", lwd=2)
        # Création du graphe pour toutes les sims :
        for (i in 1:dim(sData)[2])
        {
            lines(x=sort(rank(-sData[,i]), decreasing=T), y=sort(sData[,i], decreasing=F), col="darkgrey", lwd=1)
        }
        lines(x=sort(rank(mData), decreasing=T), y=sort(mData, decreasing=F), col="firebrick", lwd=2)
        lines(x=sort(rank(-cData), decreasing=T), y=sort(cData, decreasing=F), col="darkblue", lwd=2)
        title("Courbes Rang-Taille observées et simulées en fin de période")
        legend(x="topright", "Observé", cex=0.7, seg.len=4, col="darkblue" , lty=1, lwd=2 )
        legend(x="bottomleft", "Simulées", cex=0.7, seg.len=4, col="darkgrey" , lty=1 )
        legend(x="bottomright", "Moyenne des simulations", cex=0.7, seg.len=4, col="firebrick" , lty=1, lwd=2 )
    })
    
    output$top10 <- renderDataTable({
      dataValues$lastCensusesTable
    }, options = list(pageLength = 10, dom  = "t", order = list(5, 'desc')))
    
    
    output$sizeClasses <- renderDataTable({
        df <- dataValues$rawDF
        valBreaks <- c(0, 10E3, 50E3, 100E3, 1E6, 10E6, 10E9)
        df$Pop <- df[,input$dateClasses]
        df$N <- 1
        df$SizeClasses <- cut(df[,input$dateClasses],breaks = valBreaks, include.lowest = TRUE, right = FALSE,
                              labels = c("< 10k", "10k - 50k",
                                         "50k - 100k", "100k - 1M",
                                         "1M - 10M",  "> 10M"))
        SizeClassTable <- aggregate(df[,c("N", "Pop")],
                                    by = list(df$SizeClasses), FUN = sum, na.rm = T )
        colnames(SizeClassTable) <- c("SizeClass", "NumberOfCities", "TotalPopulation")
        SizeClassTable$ProportionOfUrbanPopulation <- SizeClassTable$TotalPopulation / sum(SizeClassTable$TotalPopulation) * 100      
        Total <- c("Total", sum(SizeClassTable$NumberOfCities), sum(SizeClassTable$TotalPopulation), 100)
        SizeClassTable <- rbind (SizeClassTable, Total)
        SizeClassTable
    }, 
    options  =  list(dom = "t"))
    
    
    output$plotZipf <- renderPlot({
      zipf <- analysisValues$zipfTable
    
    valBreaks=c(10000, 100000, 1000000, 10000000)
    
    p <-ggplot(zipf, aes(x=ranks, y=size)) 
    p + scale_y_log10(breaks=valBreaks) +
      scale_x_log10(breaks=c(1, 10, 100, 1000)) + 
      xlab("Rank") + ylab("Size (Population)") +
      geom_point(colour="aquamarine3") + geom_line(colour="aquamarine3", size = 2) +
      theme(axis.text=element_text(size=12) ,
           # axis.title=element_text(size=14),
            axis.text.x = element_text(angle = 45, hjust = 1)#,
            #ggtitle(paste("Zipf Plot in ", input$date, sep=""))
            ) 
    })
    


    output$estimZipf <- renderTable({
      exportZipfResTable()  
    },digits = 3)
    


output$plotLognormal <- renderPlot({
  pops <- exportLogNormalTable()
  LogPopulations <- log(pops$datepop)
   hist(LogPopulations, col="aquamarine3", freq=F)
  fit<-fitdistr(LogPopulations,"log-normal")$estimate
  lines(dlnorm(0:max(LogPopulations),fit[1],fit[2]), lwd=3)  
  })

output$estimLognormal <- renderTable({
  pops <- exportLogNormalTable()
  Populations <- as.data.frame(sort(pops$datepop,decreasing = TRUE))
  colnames(Populations) <- c("Pop")
 # Populations
  ln_m <- dislnorm$new(Populations$Pop)
  est_ln <- estimate_xmin(ln_m)
  ln_m$setXmin(est_ln)
 
 ln_estim = data.frame(matrix(ncol = 3, nrow = 1))
  ln_estim[1,1] <- ln_m$pars[[1]]
 ln_estim[1,2] <- ln_m$pars[[2]]
 ln_estim[1,3] <- ln_m$xmin
 colnames(ln_estim) <- c("Mean", "Standard Deviation", "X min")
 ln_estim
})

output$transitionMatrix <- renderTable({
  exportTransitionMatrix()
})  

output$transitionMatrixRel <- renderTable({
    trMatrix <-  exportTransitionMatrix()
    relMatrix <-  trMatrix  / rowSums(trMatrix) * 100
    })  
  
    output$correlations <- renderTable({
        obs <- dataValues$calcDF
        reducedSim <- simMeans()[,colnames(obs)]
        myCors <- as.vector(unlist(lapply(X=colnames(obs), FUN=function(x){
            cor(sort(obs[,x]), y=sort(reducedSim[,x]))
        })))
        
        myLogCors <- as.vector(unlist(lapply(X=colnames(obs), FUN=function(x){
            cor(sort(log(obs[,x])), y=sort(log(reducedSim[,x])))
        })))
        
        myObsSD <- as.vector(unlist(apply(X=obs, MARGIN=2, FUN=sd)))
        mySimSD <- as.vector(unlist(apply(X=reducedSim, MARGIN=2, FUN=sd)))
        myObsMean <- as.vector(unlist(apply(X=obs, MARGIN=2, FUN=mean)))
        mySimMean <- as.vector(unlist(apply(X=reducedSim, MARGIN=2, FUN=mean)))
        myObsSum <-  as.vector(unlist(apply(X=obs, MARGIN=2, FUN=sum)))
        mySimSum <-  as.vector(unlist(apply(X=reducedSim, MARGIN=2, FUN=sum)))
        
        corTable <- data.frame(row.names=colnames(obs),
                               Corr=myCors,
                               LogCorr=myLogCors,
                               ObsSum=myObsSum,
                               SimSum=mySimSum,
                               ObsMean=myObsMean,
                               SimMean=mySimMean, 
                               ObsSD=myObsSD,
                               SimSD=mySimSD)
        
        # On supprime les dates non recensement
        
        # Corrélation entre villes observées triées(à chaque date de recensement) et moyenne des réplications (idem) triée.
        # Idem en log
        # A chaque date, écart-type (ou C.V ?) simulé/observé.
    })


    
    updateInputs <- function(session, columns, realColumns){        
        updateSelectInput(session=session, inputId="timeColumnSelected",
                          choices=realColumns, selected=realColumns)
        updateSelectInput(session=session, inputId="dateClasses",
                          choices=realColumns, selected=realColumns[length(realColumns)])
    }
    
    
    
    
})

