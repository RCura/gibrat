library(shiny)
library(ggplot2)
# TODO : Add a computation of correlation for each census date observed/ mean of simulated

# Define server logic for random distribution application
shinyServer(function(input, output, session) {
    
    values <- reactiveValues(dataSource= 'none')
    
    observe({
        if (input$testData > 0){
            values$dataSource <- "data/villesFr1831.csv"
        }
    })
    
    observe({
        if (!is.null(input$csvInput)){
            values$dataSource <- input$csvInput$datapath
        }
    })
    
    upload <- reactive({
        csvPath <- values$dataSource
        if(csvPath == 'none'){return()}
        baseData <- read.csv(file=csvPath,
                             quote=input$quote,
                             sep=input$sep,
                             header=input$header,
                             dec=input$dec,
                             check.names = FALSE)
        allColumns <- c("None", unlist(colnames(baseData)))
        realColumns <- unlist(colnames(baseData[, sapply(baseData, is.numeric)]))
        
        if(csvPath == 'data/villesFr1831.csv'){
            updateTestDataInputs(session, allColumns, realColumns)
        } else {
            updateInputs(session, allColumns, realColumns)
        }

        return(baseData)   
    })
    
    
    calcData <- reactive({
        if (!is.null(upload())){
            idColumn <- input$idColumn
            timeColumns <- input$timeColumnSelected
            calcData <- upload()[timeColumns]
            calcMatrix <- as.matrix(calcData)
            calcMatrix[calcMatrix == 0] <- NA
            calcData <- as.data.frame(calcMatrix)
            row.names(calcData) <- unlist(upload()[idColumn])
            
            return(calcData)
        } else {
            return()
        }
    })
    
    
    
    computeGrowthTable <- reactive({
        if (!is.null(calcData())) {
            df <- calcData()
            #growthTable <- compute_growthtable(df)
            growthTable <- compute_yearly_growth_table(df)
            return(growthTable)
        } else {
            return()
        }
    })
    
    exportTop10Table <- reactive({
      if (!is.null(calcData())) {
        df <- calcData()
        CityNames <- rownames(df)
        lastDate <- df[,ncol(df)]
        beforelast <- df[,ncol(df)-1]
        beforebeforelast <- df[,ncol(df)-2]
        Top10Table = data.frame(CityNames, beforebeforelast, beforelast, lastDate)
        Top10Table <-   Top10Table[rev(order(Top10Table[,ncol(Top10Table)])),]
        colnames(Top10Table) <- c("Names", tail(names(df),3))
        return(Top10Table)
      } else {
        return()
      }
      
        })
    
    simulationsData <- reactive({
        if (!is.null(calcData()) && input$runSim > 0) {
            df <- calcData()
            nbReps <- isolate(input$nbReplications)
            simData <- run_simulation(df=df, reps=nbReps)
            return(simData)
        } else {
            return()
        }
    })
    
    simResults <- reactive({
        if (!is.null(simulationsData())){
            simResults <- simulationsData()[,ncol(calcData()),]
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
            meanRanks <- create_rank_tables(obsdata=calcData(), simMean=simMeans())
            return(meanRanks)
        } else {
            return()
        }
    })
    
    simRanks <- reactive({
        if (!is.null(meanRanks())) {
            simRank <- as.data.frame(meanRanks()[1])
            colnames(simRank) <- colnames(calcData())
            return(simRank)
        } else {
            return()
        }
    })
    
    obsRanks <- reactive({
        if (!is.null(meanRanks())) {
            obsRank <- as.data.frame(meanRanks()[2])
            colnames(obsRank) <- colnames(calcData())
            return(obsRank)
        } else {
            return()
        }
    })
    
    
    output$data <- renderDataTable({
        if (!is.null(calcData())) {
            df <- calcData()
            df <- cbind(upload()[input$idColumn], df)
            colnames(df)[1] <- "Name"
            return(df)
        } else {
            return()
        }
    })
    
    
    output$growthPlot <- renderPlot({
        if (!is.null(calcData())){
            growthTable <- computeGrowthTable()
            meanGrowth <- unlist(growthTable[1,])
            sdGrowth <- unlist(growthTable[2,])
            minY <- min(meanGrowth - sdGrowth) 
            maxY <- max(meanGrowth + sdGrowth)
            xLabels <- colnames(calcData())[-1]
            polyX <- c(xLabels, rev(xLabels))
            polyY <- c((meanGrowth + sdGrowth), rev(meanGrowth - sdGrowth))
            plot(y=unlist(growthTable[1,]), x=colnames(calcData())[-1],
                 ylim=c(minY, maxY), type="b", pch=4,
                 xlab="Year",
                 ylab="Growth (%)")
            polygon(polyX, polyY, col="blue", border = NA,density=50)
            
        }
        
    })
    
    exportGrowthTable <- reactive({
        if (!is.null(calcData())){
            growthTable <- computeGrowthTable()
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
    }, , options = list(iDisplayLength = 50))
    

    
    output$dlButton <- downloadHandler(
        filename = function() { paste(values$dataSource, "_growth", '.csv', sep='') },
        content = function(file) {
            write.table(x=exportGrowthTable(), file=file,sep=",", dec=".", row.names=FALSE, col.names=TRUE, quote=TRUE)
        }
    )
    
    output$simresultDL <- downloadHandler(
        filename = function() {paste(values$dataSource, "_simresults", ".csv", sep="")},
        content = function(file){
            exportDF <- simulationsData()[,ncol(calcData()),]
            exportDF <- data.frame(ID=row.names(exportDF), exportDF)
            write.table(x=exportDF, file=file, sep=",", row.names=FALSE, col.names=TRUE, quote=TRUE)
        }
        )
    
    output$gibratRankSize <- renderPlot({
        if (is.null(simMeans())){ return()}
        
        lastTime <- ncol(calcData())
        
        cData <- na.omit(calcData()[,lastTime])
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
      exportTop10Table()
    }, , options = list(iDisplayLength = 10))
    
    output$sizeClasses <- renderDataTable({
      df <- exportTop10Table()
      valBreaks <- c(0, 10000, 50000, 100000, 500000, 1000000, 10000000, 1000000000)
      if ( input$thousands == TRUE) valBreaks = valBreaks / 1000
      df$Pop <- df[,4]
      df$N <- 1
      df$SizeClasses <- cut(df[,4],breaks = valBreaks, include.lowest = TRUE, right = FALSE)
      SizeClassTable <- aggregate(df[,c("N", "Pop")],
                        by = list(df$SizeClasses), FUN = sum, na.rm = T )
      colnames(SizeClassTable) <- c("SizeClass", "NumberOfCities", "TotalPopulation")
      SizeClassTable$ProportionOfUrbanPopulation <- SizeClassTable$TotalPopulation / sum(SizeClassTable$TotalPopulation) * 100      
      Total <- c("Total", sum(SizeClassTable$NumberOfCities), sum(SizeClassTable$TotalPopulation), 100)
      SizeClassTable <- rbind (SizeClassTable, Total)
      SizeClassTable
    })
    
    output$plotZipf <- renderPlot({
      df <- exportTop10Table()
      dfZpif <- df
      
      if (input$date == "Last Census") datecol <- 4
      if (input$date == "Last Census - 1") datecol <- 3
      if (input$date == "Last Census - 2") datecol <- 2
     
      dfZpif$datepop <- dfZpif[,datecol]
      dfZpif <- subset(dfZpif, datepop > 0)
      
      sizes <- dfZpif[order(-dfZpif$datepop) , ]
      sizes <- sizes[,5]
      ncities <- nrow(dfZpif)
      ranks <- 1:ncities
      zipf = data.frame(ranks, sizes)
      colnames(zipf) <- c("ranks", "size")
      dates <- rep(names(df)[[datecol]], ncities)
      zipf = data.frame(zipf, dates)
    
    valBreaks=c(10000, 100000, 1000000, 10000000)
    if ( input$thousands == TRUE) valBreaks = valBreaks / 1000
    
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
    })
    
    output$correlations <- renderTable({
        obs <- calcData()
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
        updateSelectInput(session=session, inputId="idColumn",
                          choices=columns, selected="")
        
        updateSelectInput(session=session, inputId="timeColumnSelected",
                          choices=realColumns, selected="")
    }
    
    updateTestDataInputs <- function(session, columns, realColumns){
        updateSelectInput(session=session, inputId="idColumn",
                          choices=columns, selected=columns[2])
        
        updateSelectInput(session=session, inputId="timeColumnSelected",
                          choices=realColumns, selected=columns[-c(1,2)])
    }
    
    
    
    
})

