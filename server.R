library(shiny)

# Define server logic for random distribution application
shinyServer(function(input, output) {
    
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
                             check.names = FALSE)
        return(baseData)   
    })
    
    readData <- reactive({
        if (!is.null(upload())) {
            df <- upload()
            row.names(df) <- df[,1]
            colnames(df)[1] <- "Name"
            return(df) 
        } else { return()}    
    })
    
    calcData <- reactive({
        if (!is.null(readData())){
            df <- readData()
            calcData <- df[,-1]
            return(calcData)
        } else {
            return()
        }
    })
    
    computeGrowthTable <- reactive({
        if (!is.null(calcData())) {
            df <- calcData()
            growthTable <- compute_growthtable(df)
            return(growthTable)
        } else {
            return()
        }
    })
    
    simulationsData <- reactive({
        if (!is.null(calcData()) && input$runSim > 0) {
            df <- calcData()
            nbReps <- isolate(input$nbReplications)
            print(nbReps)
            simData <- run_simulation(df=df, reps=nbReps)
            dimnames(simData) <- list(rownames(df), colnames(df), paste("Sim", 1:nbReps, sep=""))
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
            simMeans <- apply(X=simulationsData()[,,], 1:2, mean)
            return(simMeans)
        } else {
            return()
        }
    })
    
    simSDs <- reactive({
        if (!is.null(simulationsData())){
            simSDs <- apply(X=simulationsData()[,,], 1:2, sd)
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
    
    correlationMatrix <- reactive({
        if (!is.null(simulationsData())){
            correlationMatrix <- as.matrix(unlist(lapply(1:dim(simulationsData())[[3]],
                                                         function (x) return(
                                                             cor(log(calcData()[ncol(calcData())]),
                                                                 y=log(simulationsData()[,ncol(calcData()),x]))))))
            dimnames(correlationMatrix) <- dimnames(simulationsData())[3]
            return(correlationMatrix)
        } else {
            return()
        }
    })
    
    rowCorrelationMatrix <- reactive({
        if (!is.null(simulationsData())){
            rowCorrelationMatrix <- matrix(data=unlist(lapply(
                X=1:nrow(calcData()),
                FUN= function (x) return(cor(x=simulationsData()[x,,1:dim(simulationsData())[[3]]],
                                             y=as.double(calcData()[x,]),
                                             use="pairwise.complete.obs",
                                             method="spearman")))),
                                           nrow=nrow(calcData()),
                                           ncol=dim(simulationsData())[[3]],
                                           byrow=TRUE,
                                           dimnames=list(rownames(calcData()),
                                                         colnames(simResults())))
            return(rowCorrelationMatrix)
        } else {
            return()
        }
    })
    
    output$data <- renderDataTable({
        if (!is.null(readData())) {
            readData()
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
                 ylim=c(minY, maxY), type="b", pch=4, col.points="black",
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
            return(tGrowthTable)
        } else {
            return()
        }
    })
    
    output$growthTable <- renderDataTable({
        exportGrowthTable()
    })
    

    
    output$dlButton <- downloadHandler(
        filename = function() { paste(values$dataSource, "_growth", '.csv', sep='') },
        content = function(file) {
            write.table(x=exportGrowthTable(), file=file,sep=",", dec=".", row.names=FALSE, col.names=TRUE, quote=TRUE)
        }
    )
    
    
    output$gibratRankSize <- renderPlot({
        if (is.null(simMeans())){ return()}
        lastTime <- ncol(calcData())
        maxpop <- max(max(calcData()[lastTime]),max(simulationsData()[,lastTime,]))
        minpop <- min(min(calcData()[lastTime]),min(simulationsData()[,lastTime,]))
        
        plot(x=sort(rank(-calcData()[,lastTime]), decreasing=T), y=sort(calcData()[,lastTime], decreasing=F), log="xy", type="l", ylim=c(minpop,maxpop), xlab="Rang", ylab="Population", col="darkblue", lwd=2)
        # Création du graphe pour toutes les sims :
        for (i in 1:dim(simulationsData())[3])
        {
            lines(x=sort(rank(-simulationsData()[,lastTime,i]), decreasing=T), y=sort(simulationsData()[,lastTime,i], decreasing=F), col="darkgrey", lwd=1)
        }
        lines(x=sort(rank(-simMeans()[,lastTime]), decreasing=T), y=sort(simMeans()[,lastTime], decreasing=F), col="firebrick", lwd=2)
        lines(x=sort(rank(-calcData()[,lastTime]), decreasing=T), y=sort(calcData()[,lastTime], decreasing=F), col="darkblue", lwd=2)
        title("Courbes Rang-Taille observées et simulées en fin de période")
        legend(x="topright", "Observé", cex=0.7, seg.len=4, col="darkblue" , lty=1, lwd=2 )
        legend(x="bottomleft", "Simulées", cex=0.7, seg.len=4, col="darkgrey" , lty=1 )
        legend(x="bottomright", "Moyenne des simulations", cex=0.7, seg.len=4, col="firebrick" , lty=1, lwd=2 )
    })
    
    
    output$correlationsFigures <- renderPrint({
        if (!is.null(correlationMatrix())){
            "Corrélation ville obs/ville sim pour chaque réplication, résumé par réplication."
            tags$hr()
            summary(rowCorrelationMatrix())
            tags$hr()
            summary(rowCorrelationMatrix())
        } else {
            return()
        }
    })
    
    
})

