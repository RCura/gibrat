library(shiny)
library(ggplot2)
library(MASS)
library(poweRlaw)
library(DT)
library(reshape2)
library(dplyr)
library(parallel)
library(moments)
library(xtable)

library(sp)
library(cartography)
library(maps)
library(markdown)

#library(shinyURL)

shinyServer(function(input, output, session) {
    #shinyURL.server()
    #options(warn=0, error=browser, shiny.error=browser)
    load("data/countriesPop.RData")
    
    dataValues <- reactiveValues(
        rawDF = NULL,
        filtredDF = NULL,
        calcDF = NULL,
        growthTable = NULL,
        rawGrowthTable = NULL,
        rawGrowthTableSizeInit = NULL,
        lastCensusesTable  = NULL
    )
    
    
    computedValues <- reactiveValues(
        simData = NULL,
        simResults = NULL,
        simMeans = NULL,
        simSDs = NULL,
        meanRanks = NULL,
        simRanks = NULL,
        obsRanks = NULL
    )
    
    analysisValues <- reactiveValues(
        zipfTable = NULL,
        transitionMatrix = NULL,
        logNormalTable = NULL,
        fittedLogNormalTable  = NULL,
        zipfResTable = NULL,
        corGrowthSize = NULL,
        corTemporal  =  NULL
    )
    
    
    observe({
        countryName <- input$dataset
        resetValues()
        if (countryName ==  "Brazil") {
            dataValues$rawDF <- Brazil
        } else if (countryName ==  "Russia") {
            dataValues$rawDF <- Russia
        } else  if (countryName ==  "India") {
            dataValues$rawDF <- India
        }  else if (countryName ==  "China") {
            dataValues$rawDF <- China
        } else if (countryName == "China (Historical)"){
            dataValues$rawDF <- China_Historic  
        } else  if (countryName ==  "South Africa") {
            dataValues$rawDF <- SouthAfrica
        } else if (countryName ==  "USA") {
            dataValues$rawDF <- USA
        } else  {
            dataValues$rawDF <- Europe
        }
        
        timeColumns <-
            suppressWarnings(as.numeric(na.omit(as.numeric(
                unlist(colnames(dataValues$rawDF))
            ))))
        allColumns <- c("None", unlist(colnames(dataValues$rawDF)))
        updateInputs(session, allColumns, timeColumns)
        
    })
    
    observe({
        timeColumns <- input$timeColumnSelected
        if (!is.null(timeColumns) &&
            all(timeColumns %in% colnames(dataValues$rawDF))) {
            calcData <- dataValues$rawDF[, timeColumns]
            row.names(calcData) <- dataValues$rawDF$ID
            dataValues$calcDF <- calcData
        }
    })
    
    observe({
        if (!is.null(dataValues$calcDF)) {
            YGT <- compute_yearly_growth_table(dataValues$calcDF)
            dataValues$growthTable <- YGT$Summary
            dataValues$rawGrowthTable <-  YGT$fullDF
            dataValues$rawGrowthTableSizeInit <-
                YGT$growthRateSizeInitial
        }
    })
    
    output$rawGrowthTable <- renderDataTable({
        dataValues$rawGrowthTable
    })
    
    output$rawGrowthTableSizeInit <- renderDataTable({
        dataValues$rawGrowthTableSizeInit
    })
    
    observe({
        if (!is.null(dataValues$rawGrowthTable)) {
            growthDF <- dataValues$rawGrowthTable
            popDF <-  dataValues$rawGrowthTableSizeInit
            periodNames <-  colnames(growthDF)
            resultDF <-
                data.frame(
                    Period = NA,
                    Correlation  =  NA,
                    LogCorrelation = NA,
                    NbCities  =  NA,
                    stringsAsFactors = FALSE
                )
            resultDF <- resultDF[-1, ]
            for (currCol in 1:ncol(growthDF)) {
                currentPeriod <-  colnames(growthDF)[currCol]
                linCor <-
                    cor(
                        x = growthDF[, currCol],
                        y = popDF[, currCol],
                        use = "pairwise.complete.obs",
                        method = "pearson"
                    )
                logCor <-
                    cor(
                        x = growthDF[, currCol],
                        y = log(popDF[, currCol]),
                        use = "pairwise.complete.obs",
                        method = "pearson"
                    )
                numCities  <- nrow(popDF[!is.na(popDF[, currCol]), ])
                resultDF[nrow(resultDF)  +  1, ] <-
                    c(currentPeriod, linCor, logCor, numCities)
            }
            analysisValues$corGrowthSize <- resultDF
        }
    })
    
    output$correlSizeGrowth <- renderDataTable({
        datatable(
            analysisValues$corGrowthSize,
            rownames  = FALSE,
            options = list(dom = 't')
        )  %>% formatRound(c('Correlation', 'LogCorrelation'), 3)
    })
    
    output$correlSizeGrowthPlot <- renderPlot({
        if (!is.null(analysisValues$corGrowthSize)) {
            resultDF <- analysisValues$corGrowthSize
            resultDF$Period <-
                colnames(dataValues$rawGrowthTableSizeInit)
            resultDF[c(1:4)] <- sapply(resultDF[c(1:4)], as.numeric)
            plot(
                x = resultDF[, 1],
                y =  resultDF[, 2],
                ylim = c(min(resultDF[, 2:3]), max(resultDF[, 2:3])),
                main  = "Correlation Growth & Population size",
                xlab = "Initial period",
                ylab =  "Correlation coefficient (Pearson)",
                type  = "b",
                col = "red"
            )
            lines(
                x = resultDF[, 1],
                y =  resultDF[, 3],
                type  = "b",
                col = "orange"
            )
            abline(h =  0, lty =  2)
            legend(
                x = "bottomleft",
                legend = c(
                    "Growth rate & Population",
                    "Growth rate  & log(Population)"
                ),
                cex = 1,
                seg.len = 2,
                col = c('red', "orange"),
                pch = NA,
                lty = 1,
                lwd = 1
            )
        }
        
    })
    
    
    observe({
        if (!is.null(dataValues$rawGrowthTable)) {
            growthTable <-  dataValues$rawGrowthTable
            resultDF  <-
                data.frame(
                    Period  = NA,
                    Label = NA,
                    Correlation  = NA,
                    NbCities =  NA,
                    check.names  =  FALSE,
                    stringsAsFactors = FALSE
                )
            resultDF <-  resultDF[-1, ]
            for (currCol  in  (1:(ncol(growthTable) - 1))) {
                currPeriod <-
                    paste(colnames(growthTable)[currCol],
                          colnames(growthTable)[currCol + 1],
                          sep = " -> ")
                currLabel <- as.character(as.roman(currCol))
                currCor <-
                    cor(
                        x = growthTable[, currCol],
                        y = growthTable[, currCol + 1],
                        use = "pairwise.complete.obs",
                        method = "pearson"
                    )
                currNb <-
                    nrow(na.omit(growthTable[, c(currCol, currCol + 1)]))
                resultDF[nrow(resultDF) + 1, ] <-
                    c(currPeriod, currLabel, currCor, currNb)
            }
            analysisValues$corTemporal <-  resultDF
        }
    })
    
    output$correlTemporal <- renderDataTable({
        datatable(
            analysisValues$corTemporal,
            rownames = FALSE,
            options = list(dom = 't')
        ) %>% formatRound('Correlation', 3)
    })
    
    output$correlTemporalPlot <-  renderPlot({
        if (!is.null(dataValues$rawGrowthTable)) {
            baseDF <-  analysisValues$corTemporal
            plot(
                x = as.roman(baseDF$Label),
                y = as.numeric(baseDF$Correlation),
                xaxt  = "n",
                type  = "b",
                col = "red",
                xlab  = "Period",
                ylab  = "Correlation coefficient (Pearson)",
                main = "Temporal auto-correlation"
            )
            axis(
                side = 1,
                at = as.numeric(as.roman(baseDF$Label)),
                labels = baseDF$Label
            )
            abline(h = 0, lty = 2)
            legend(
                x = "bottomleft",
                legend =  paste(baseDF$Label,  baseDF$Period, sep  =
                                    " : "),
                seg.len = 0,
                col = "black",
                pch = NA,
                lty = 0,
                lwd = 0
            )
        }
    })
    
    observe({
        if (!is.null(dataValues$calcDF)) {
            df <- dataValues$rawDF
            shortDF <- df[, c(1:2, (ncol(df) - 2):ncol(df))]
            dataValues$lastCensusesTable <-
                shortDF[order(shortDF[, -ncol(shortDF)]), ]
        }
    })
    
    observe({
        if (!is.null(dataValues$rawDF) &&
            input$dateZipf !=  "" &&
            input$dateZipf %in% names(dataValues$rawDF)) {
            dfZipf <- dataValues$rawDF
            dfZipf <-
                as.data.frame(dfZipf[dfZipf[, input$dateZipf] > 0, input$dateZipf], check.names =  FALSE)
            dfZipf <- dfZipf[order(-dfZipf[, 1]),]
            ranks <- 1:length(dfZipf)
            zipf <-
                data.frame(ranks,
                           dfZipf,
                           stringsAsFactors =  FALSE,
                           check.names = FALSE)
            colnames(zipf) <- c("ranks", "size")
            zipf$dates <- names(dataValues$rawDF)[input$dateZipf]
            analysisValues$zipfTable <- zipf
        }
    })
    
    observe({
        if (!is.null(dataValues$rawDF) &&
            input$dateFinal  != "" && input$dateInitial  != "" &&
            input$dateFinal %in% names(dataValues$rawDF)  &&
            input$dateInitial %in% names(dataValues$rawDF)) {
            df <- dataValues$rawDF
            valBreaks <- c(0, 10E3, 50E3, 100E3, 1E6, 10E6, 10E9)
            labelsBreaks <-
                c("< 10k",
                  "10k - 50k",
                  "50k - 100k",
                  "100k - 1M",
                  "1M - 10M",
                  "> 10M")
            FinalPops <- df[, input$dateFinal]
            InitialPops <- df[, input$dateInitial]
            FinalDate <-
                cut(
                    x = FinalPops,
                    breaks = valBreaks,
                    include.lowest = TRUE,
                    right = FALSE,
                    labels = labelsBreaks
                )
            InitialDate <-
                cut(
                    x = InitialPops,
                    breaks = valBreaks,
                    include.lowest = TRUE,
                    right = FALSE,
                    labels = labelsBreaks
                )
            transitionMatrix <- table(InitialDate, FinalDate)
            analysisValues$transitionMatrix <-  transitionMatrix
        }
    })
    
    observe({
        if (!is.null(dataValues$rawDF) &&
            input$dateLogNormal  != "" &&
            input$dateLogNormal %in% names(dataValues$rawDF)) {
            dfLG <-  dataValues$rawDF
            dfLG$datepop  <-  dataValues$rawDF[, input$dateLogNormal]
            dfLG <- subset(dfLG, datepop > 10E3)
            analysisValues$logNormalTable <- dfLG
            
        }
    })
    
    observe({
        if (!is.null(analysisValues$zipfTable)) {
            zipf <- analysisValues$zipfTable
            zipfCut10 <- subset(zipf, size >= 10000)
            zipfCut100 <- subset(zipf, size >= 100000)
            
            model10 <-
                lm(log(size) ~ log(ranks),
                   data = zipfCut10,
                   na.action = na.omit)
            ConfInt_model10 <- confint(model10)
            res10 = data.frame(
                model10$coefficients[[2]],
                ConfInt_model10[2, 1],
                ConfInt_model10[2, 2],
                summary(model10)$r.squared
            )
            colnames(res10) <-
                c("Estimated Zipf Exponent",
                  "Lower bound",
                  "Upper bound",
                  "R squared")
            
            model100 <-
                lm(log(size) ~ log(ranks),
                   data = zipfCut100,
                   na.action = na.omit)
            ConfInt_model100 <- confint(model100)
            res100 = data.frame(
                model100$coefficients[[2]],
                ConfInt_model100[2, 1],
                ConfInt_model100[2, 2],
                summary(model100)$r.squared
            )
            colnames(res100) <-
                c("Estimated Zipf Exponent",
                  "Lower bound",
                  "Upper bound",
                  "R squared")
            
            res <- rbind(res10, res100)
            Estimation  <-
                c("Population > 10000 hab.", "Population > 100000 hab.")
            res <- cbind(Estimation, res)
            analysisValues$zipfResTable <- res
        }
        
    })
    
    observe({
        if (!is.null(dataValues$calcDF) && input$runSim > 0) {
            df <- dataValues$calcDF
            isolate({
                nbReps <- input$nbReplications
                computedValues$simData <-
                    run_simulation(df = df, reps = nbReps)
                computedValues$simResults <-
                    computedValues$simData[, ncol(dataValues$calcDF), ]
                computedValues$simMeans <-
                    apply(X = computedValues$simData[, , ], 1:2, function(x) {
                        mean(x, na.rm = TRUE)
                    })
                computedValues$simSDs <-
                    apply(X = computedValues$simData[, , ], 1:2, function(x) {
                        return(sd(x, na.rm = TRUE))
                    })
                computedValues$meanRanks <-
                    meanRanks <-
                    create_rank_tables(obsdata = dataValues$calcDF,
                                       simMean = computedValues$simMeans)
                
                computedValues$simRanks <-
                    as.data.frame(computedValues$meanRanks[1])
                colnames(computedValues$simRanks) <-
                    colnames(dataValues$calcDF)
                
                computedValues$obsRanks <-
                    as.data.frame(computedValues$meanRanks[2])
                colnames(computedValues$obsRanks) <-
                    colnames(dataValues$calcDF)
            })
        }
    })
    
    
    output$data <- renderDataTable({
        if (!is.null(dataValues$rawDF)) {
            dataValues$rawDF
        }
    }, rownames  = FALSE,
    filter = "bottom",
    extensions = list(FixedColumns = list(leftColumns = 2)),
    options = list(scrollX = TRUE,
                   scrollCollapse = TRUE),
    class = 'cell-border stripe')
    
    
    output$growthPlot <- renderPlot({
        if (!is.null(dataValues$calcDF)) {
            growthTable <- dataValues$growthTable
            meanGrowth <- unlist(growthTable[1, ])
            sdGrowth <- unlist(growthTable[2, ])
            minY <- min(meanGrowth - sdGrowth)
            maxY <- max(meanGrowth + sdGrowth)
            xLabels <- colnames(dataValues$calcDF)[-1]
            polyX <- c(xLabels, rev(xLabels))
            polyY <-
                c((meanGrowth + sdGrowth), rev(meanGrowth - sdGrowth))
            plot(
                y = unlist(growthTable[1, ]),
                x = colnames(dataValues$calcDF)[-1],
                ylim = c(minY, maxY),
                type = "b",
                pch = 4,
                xlab = "Year",
                ylab = "Growth (%)"
            )
            polygon(
                polyX,
                polyY,
                col = "blue",
                border = NA,
                density = 50
            )
            
        }
        
    })
    
    exportGrowthTable <- reactive({
        if (!is.null(dataValues$calcDF)) {
            growthTable <- dataValues$growthTable
            periodNames <- colnames(growthTable)
            indicatorNames <- row.names(growthTable)
            tGrowthTable <-
                cbind.data.frame(periodNames, t(growthTable))
            colnames(tGrowthTable) <- c("Period", indicatorNames)
            #View(tGrowthTable)
            return(tGrowthTable)
        } else {
            return()
        }
    })
    
    output$growthTable <- renderDataTable({
        exportGrowthTable()
    }, rownames = FALSE, options = list(length = 50, dom = "t"))
    
    
    
    output$dlButton <- downloadHandler(
        filename = function() {
            paste(input$dataset, "_growth", '.csv', sep = '')
        },
        content = function(file) {
            write.table(
                x = exportGrowthTable(),
                file = file,
                sep = ",",
                dec = ".",
                row.names = FALSE,
                col.names = TRUE,
                quote = TRUE
            )
        }
    )
    
    output$simresultDL <- downloadHandler(
        filename = function() {
            paste(input$dataset, "_simresults", ".csv", sep = "")
        },
        content = function(file) {
            exportDF <- computedValues$simData[, ncol(dataValues$calcDF), ]
            exportDF <- data.frame(ID = row.names(exportDF), exportDF)
            write.table(
                x = exportDF,
                file = file,
                sep = ",",
                row.names = FALSE,
                col.names = TRUE,
                quote = TRUE
            )
        }
    )
    
    output$gibratRankSize <- renderPlot({
        if (is.null(computedValues$simMeans)) {
            return()
        }
        
        lastRealTime <- ncol(dataValues$calcDF)
        lastSimTime <- dim(computedValues$simData)[2]
        
        sData <- computedValues$simData[, lastSimTime, ]
        
        cData <- dataValues$calcDF[, lastRealTime]
        if (!input$showNonSimulated) {
            cData <- cData[!is.na(sData[, 1])]
        }
        
        sData <- na.omit(sData)
        cData <- na.omit(cData)
        mData <- na.omit(computedValues$simMeans[, lastSimTime])
        
        maxpop <- max(max(cData), max(sData))
        minpop <- min(min(cData), min(sData))
        
        plot(
            x = sort(rank(-cData), decreasing = T),
            y = sort(cData, decreasing = FALSE),
            log = "xy",
            type = "l",
            ylim = c(minpop, maxpop),
            xlab = "Rang",
            ylab = "Population",
            col = "darkblue",
            lwd = 2
        )
        # Création du graphe pour toutes les sims :
        for (i in 1:dim(sData)[2])
        {
            lines(
                x = sort(rank(-sData[, i]), decreasing = TRUE),
                y = sort(sData[, i], decreasing = FALSE),
                col = "darkgrey",
                lwd = 1
            )
        }
        lines(
            x = sort(rank(mData), decreasing = TRUE),
            y = sort(mData, decreasing = FALSE),
            col = "firebrick",
            lwd = 2
        )
        lines(
            x = sort(rank(-cData), decreasing = TRUE),
            y = sort(cData, decreasing = FALSE),
            col = "darkblue",
            lwd = 2
        )
        title("Courbes Rang-Taille observées et simulées en fin de période")
        legend(
            x = "topright",
            "Observé",
            cex = 0.7,
            seg.len = 4,
            col = "darkblue" ,
            lty = 1,
            lwd = 2
        )
        legend(
            x = "bottomleft",
            "Simulées",
            cex = 0.7,
            seg.len = 4,
            col = "darkgrey" ,
            lty = 1
        )
        legend(
            x = "bottomright",
            "Moyenne des simulations",
            cex = 0.7,
            seg.len = 4,
            col = "firebrick" ,
            lty = 1,
            lwd = 2
        )
    })
    
    output$gibratExpectation <- renderPlot({
        if (is.null(computedValues$simMeans)) {
            return()
        }
        lastObsTime <- ncol(dataValues$calcDF)
        lastSimTime <- dim(computedValues$simMeans)[2]
        
        obsData <- dataValues$calcDF[, lastObsTime]
        simData <- computedValues$simMeans[, lastSimTime]
        
        if (!input$showNonSimulated) {
            obsData <- obsData[!is.na(simData)]
            simData <- na.omit(simData)
        }
        
        minData <- min(obsData, simData, na.rm = TRUE)
        maxData <- max(obsData, simData, na.rm = TRUE)
        
        plot(
            x = obsData,
            y = simData,
            log = "xy",
            type = "p",
            xlim = c(minData, maxData),
            ylim = c(minData, maxData),
            xlab = "Observed Pop",
            ylab = "Simulated Pop",
            col = "firebrick"
        )
        abline(a = 0, b = 1, col = "black")
        
    })
    
    output$meanEvolution <- renderPlot({
        if (is.null(computedValues$simMeans)) {
            return()
        }
        obsData <- dataValues$calcDF
        
        simData <-
            as.data.frame(computedValues$simMeans, check.names = FALSE)
        simData <-
            simData[, colnames(simData) %in% colnames(obsData)]
        
        # FIXME : Ugly, to be changed
        if (!input$showNonSimulated) {
            for (i in 1:nrow(obsData)) {
                for (j in 1:ncol(obsData)) {
                    obsData[i, j] <- ifelse(is.na(simData[i, j]), NA, obsData[i, j])
                }
            }
        }
        
        #print(str(simData))
        obsMean <- unlist(lapply(obsData, mean, na.rm = TRUE))
        simMean <- unlist(lapply(simData, mean, na.rm = TRUE))
        maxData <- max(obsMean, simMean)
        minData <- min(obsMean, simMean)
        plot(
            x = colnames(obsData),
            y = obsMean,
            ylim = c(minData, maxData),
            type = "b",
            col = "darkblue",
            log = "y",
            xlab = "Year",
            ylab = "Mean Population (log)"
        )
        lines(
            x = colnames(obsData),
            y = simMean,
            type = "b",
            col = "firebrick"
        )
        legend(
            x = "bottomright",
            c("Observed mean", "Simulated mean"),
            cex = 0.7,
            seg.len = 4,
            col = c("darkblue", "firebrick") ,
            lty = 1,
            lwd = 2
        )
        
    })
    
    output$sdEvolution <- renderPlot({
        if (is.null(computedValues$simMeans)) {
            return()
        }
        obsData <- dataValues$calcDF
        simData <-
            as.data.frame(computedValues$simMeans, check.names = FALSE)
        simData <-
            simData[, colnames(simData) %in% colnames(obsData)]
        
        # FIXME : Ugly, to be changed
        if (!input$showNonSimulated) {
            for (i in 1:nrow(obsData)) {
                for (j in 1:ncol(obsData)) {
                    obsData[i, j] <- ifelse(is.na(simData[i, j]), NA, obsData[i, j])
                }
            }
        }
        
        
        obsSD <- unlist(lapply(obsData, sd, na.rm = TRUE))
        simSD <- unlist(lapply(simData, sd, na.rm = TRUE))
        maxData <- max(obsSD, simSD)
        minData <- min(obsSD, simSD)
        plot(
            x = colnames(obsData),
            y = obsSD,
            ylim = c(minData, maxData),
            type = "b",
            col = "darkblue",
            log = "y",
            xlab = "Year",
            ylab = "Population stdev (log)"
        )
        lines(
            x = colnames(obsData),
            y = simSD,
            type = "b",
            col = "firebrick"
        )
        legend(
            x = "bottomright",
            c("Pop. observed stdev", "Pop. simulated stdev"),
            cex = 0.7,
            seg.len = 4,
            col = c("darkblue", "firebrick") ,
            lty = 1,
            lwd = 2
        )
        
    })
    #     column(6, plotOutput('meanEvolution')),
    #     column(6, plotOutput('sdEvolution'))
    
    output$top10 <- renderDataTable({
        dataValues$lastCensusesTable
    }, rownames = FALSE, options = list(
        pageLength = 10,
        dom  = "t",
        order = list(4, 'desc')
    ))
    
    
    output$sizeClasses <- renderDataTable({
        df <- dataValues$rawDF
        valBreaks <- c(0, 10E3, 50E3, 100E3, 1E6, 10E6, 10E9)
        df$Pop <- df[, input$dateClasses]
        df$N <- 1
        df$SizeClasses <-
            cut(
                df[, input$dateClasses],
                breaks = valBreaks,
                include.lowest = TRUE,
                right = FALSE,
                labels = c(
                    "< 10k",
                    "10k - 50k",
                    "50k - 100k",
                    "100k - 1M",
                    "1M - 10M",
                    "> 10M"
                )
            )
        SizeClassTable <- aggregate(
            df[, c("N", "Pop")],
            by = list(df$SizeClasses),
            FUN = sum,
            na.rm = T
        )
        colnames(SizeClassTable) <-
            c("SizeClass", "NumberOfCities", "TotalPopulation")
        SizeClassTable$ProportionOfUrbanPopulation <-
            SizeClassTable$TotalPopulation / sum(SizeClassTable$TotalPopulation)
        Total <-
            c(
                "Total",
                sum(SizeClassTable$NumberOfCities),
                sum(SizeClassTable$TotalPopulation),
                1
            )
        SizeClassTable <- rbind (SizeClassTable, Total)
        datatable(SizeClassTable,
                  rownames = FALSE,
                  options = list(dom = 't')) %>%  formatPercentage("ProportionOfUrbanPopulation", 3)
    })
    
    
    output$plotZipf <- renderPlot({
        zipf <- analysisValues$zipfTable %>%
            filter(!is.na(size), size >= 10E3)
        
        
        
        valBreaks = c(10E3, 100E3, 1E6, 10E6)
        
        p <- ggplot(zipf, aes(x = ranks, y = size))
        p + scale_y_log10(breaks = valBreaks) +
            scale_x_log10(breaks = c(1, 10, 100, 1000)) +
            xlab("Rank") + ylab("Size (Population)") +
            geom_point(colour = "aquamarine3") + geom_line(colour = "aquamarine3", size = 2) +
            theme(axis.text = element_text(size = 12) ,
                   axis.title=element_text(size=14),
                  axis.text.x = element_text(angle = 45, hjust = 1)) +
            ggtitle(paste("Zipf Plot in ", input$date, sep=""))
    })
        
        
        
        output$estimZipf <- renderTable({
            analysisValues$zipfResTable
        }, digits = 3)
        
        
        
        output$plotLognormal <- renderPlot({
            pops <- analysisValues$logNormalTable
            basePops <- pops$datepop[pops$datepop > 10E3]
            skewedPops <- basePops - 10E3
            logSkewedPops <-  log(skewedPops)
            meanLog <- mean(logSkewedPops)
            sdLog <- sd(logSkewedPops)
            labels <-  c(10000, 15E3, 25E3, 50E3, 100E3, 1E6, 10E6, 20E6)
            ticks <- log(labels - 10E3)
            hist(
                logSkewedPops,
                breaks = 50,
                prob = TRUE,
                xaxt  = "n",
                main =  "Histogram of populations\n(cut at Χ₀ =  10E3)",
                xlab = "Population (log-scale)",
                ylab = "Density"
            )
            axis(
                side = 1,
                at = ticks,
                labels = labels,
                las = 1
            )
            #text(cex=.8, x=ticks, y=-0.02, labels, xpd=TRUE, srt=45, pos=1)
            lines(
                dnorm(
                    x = 0:max(logSkewedPops),
                    mean = meanLog,
                    sd = sdLog
                ),
                type = "l",
                col = "blue",
                lwd = 2
            )
        })
        
        output$qqplotLognormal <-  renderPlot({
            pops <- analysisValues$logNormalTable
            basePops <- pops$datepop[pops$datepop > 10E3]
            skewedPops <- basePops - 10E3
            logSkewedPops <-  log(skewedPops)
            
            labels <-  c(10000, 15E3, 25E3, 50E3, 100E3, 1E6, 10E6, 20E6)
            ticks <- log(labels - 10E3)
            
            qqnorm(
                y = logSkewedPops,
                xaxt = "n",
                yaxt  = "n",
                type = "p",
                pch = 19,
                cex = 1,
                col = rgb(0, 0, 1, 0.1)
            )
            qqline(logSkewedPops)
            axis(
                side = 1,
                at = ticks,
                labels = labels,
                las = 1,
                cex.axis = 0.6
            )
            axis(
                side = 2,
                at = ticks,
                labels = labels,
                las = 1,
                cex.axis = 0.6
            )
            
        })
        
        output$logNormalSummary <- renderDataTable({
            pops <- analysisValues$logNormalTable
            basePops <- pops$datepop[pops$datepop > 10E3]
            skewedPops <- basePops - 10E3
            logSkewedPops <-  log(skewedPops)
            meanLog <- mean(logSkewedPops)
            sdLog <- sd(logSkewedPops)
            if (length(logSkewedPops) > 5000) {
                testData <- sample(x = logSkewedPops, size = 5000)
            } else {
                testData <-  logSkewedPops
            }
            resultDF <- data.frame(
                method = "Shapiro-Wilk (Χ₀ =  10E3)",
                nbCities = length(testData),
                meanLog = meanLog,
                sdLog = sdLog,
                p.value = shapiro.test(testData)$p.value,
                stringsAsFactors = FALSE,
                check.names = FALSE
            )
            # See here : http://abcdr.guyader.pro/676-comment-faire-un-test-de-normalite-avec-r-le-test-de-shapiro-wilk/
            KStest <- ks.test(logSkewedPops, "pnorm")
            resultDF[2, ] <- c(
                "Kolmogorov-Smirnoff (Χ₀ =  10E3)",
                length(logSkewedPops),
                meanLog,
                sdLog,
                KStest$p.value
            )
            resultDF
        }, rownames = FALSE, options = list(dom = 't'))
        
        observeEvent(input$runLogNormal, {
            pops <- analysisValues$logNormalTable
            Populations <-
                as.data.frame(sort(pops$datepop, decreasing = TRUE))
            colnames(Populations) <- c("Pop")
            # Populations
            ln_m <- dislnorm$new(Populations$Pop)
            print(ln_m)
            est_ln <- estimate_xmin(m = ln_m)
            ln_m$setXmin(est_ln)
            print(ln_m)
            
            ln_estim = data.frame(matrix(ncol = 3, nrow = 1))
            ln_estim[1, 1] <- ln_m$pars[[1]]
            ln_estim[1, 2] <- ln_m$pars[[2]]
            ln_estim[1, 3] <- ln_m$xmin
            colnames(ln_estim) <-
                c("Mean", "Standard Deviation", "X min")
            print(ln_estim)
            analysisValues$fittedLogNormalTable  <-  ln_estim
        })
        
        output$estimLognormal <- renderTable({
            if (!is.null(analysisValues$fittedLogNormalTable)) {
                analysisValues$fittedLogNormalTable
            }
        })
        
        
        
        output$transitionMatrix <- renderTable({
            analysisValues$transitionMatrix
        })
        
        output$transitionMatrixRel <- renderTable({
            trMatrix <-  analysisValues$transitionMatrix
            relMatrix <-  trMatrix  / rowSums(trMatrix) * 100
        })
        
        
        output$zipfEvolution <- renderPlot({
            if (!is.null(dataValues$calcDF)) {
                plotData <- dataValues$calcDF
                rankSize <- plotRankSize(plotData)
                print(rankSize)
            }
        })
        
        output$zipfEvolutionSummary <-  renderDataTable({
            if (!is.null(dataValues$calcDF)) {
                SumTable <-
                    data.frame(
                        Year  =  NA,
                        Slope  = NA,
                        R2  = NA,
                        LowerBound  =  NA,
                        UpperBound  =  NA,
                        NbCities  = NA
                    )
                SumTable <-  SumTable[-1, ]
                for (currentTime  in  colnames(dataValues$calcDF)) {
                    dfZipf <-  dataValues$calcDF
                    dfZipf <-
                        as.data.frame(dfZipf[dfZipf[, currentTime] > 10E3, currentTime], check.names =  FALSE)
                    dfZipf <-  na.omit(dfZipf)
                    dfZipf <- dfZipf[order(-dfZipf[, 1]),]
                    ranks <- 1:length(dfZipf)
                    zipf <-
                        data.frame(
                            ranks,
                            dfZipf,
                            stringsAsFactors =  FALSE,
                            check.names = FALSE
                        )
                    colnames(zipf) <- c("ranks", "size")
                    myLM <-
                        lm(log(size)  ~  log(ranks),
                           data = zipf,
                           na.action = na.omit)
                    myConfInt <- confint(myLM, level = 0.95)
                    currentResults <-  c(
                        currentTime,
                        myLM$coefficients[[2]],
                        summary(myLM)$adj.r.squared,
                        myConfInt[2, 1],
                        myConfInt[2, 2],
                        nrow(zipf)
                    )
                    SumTable[nrow(SumTable) + 1, ] <- currentResults
                }
                datatable(SumTable,
                          rownames = FALSE,
                          options = list(dom = 't'))  %>% formatRound('R2', 3)  %>% formatRound(c("Slope", "LowerBound", "UpperBound"), 3)
            }
        })
        
        shapeComparisonPlot <- reactive({
            maxyear <- BRICS %>%
                group_by(system) %>%
                summarise(yearmax = max(year))
            
            lastPops <- BRICS %>%
                semi_join(maxyear, by = c("system",  "year" = "yearmax")) %>%
                filter(pop > 10E3,!is.na(pop)) %>%
                mutate(logpop = log(pop)) %>%
                mutate(sklogpop = log(pop - 10E3))
            lastPops$sysYear <-
                paste(lastPops$system, "\n(",  lastPops$year, ")", sep = "")
            
            myLogNorm <-
                function(x, mean, sd) {
                    dnorm(x, mean = mean, sd = sd)
                }
            
            minX <- min(lastPops$sklogpop)
            maxX <- max(lastPops$sklogpop)
            
            xyDF <-
                data.frame(
                    system = character(),
                    x = numeric(),
                    y = numeric(),
                    stringsAsFactors = FALSE
                )
            
            for (currentSysYear in unique(lastPops$sysYear)) {
                currentPops <- lastPops %>%
                    filter(sysYear == currentSysYear)
                
                meanLog <- mean(currentPops$sklogpop, na.rm = TRUE)
                sdLog <- sd(currentPops$sklogpop, na.rm = TRUE)
                
                myCurve <- data.frame(
                    sysYear = currentSysYear,
                    as.data.frame(curve(
                        expr = myLogNorm(x, meanLog, sdLog),
                        xlim = c(minX, maxX)
                    )),
                    stringsAsFactors = FALSE
                )
                xyDF <- xyDF %>%
                    bind_rows(myCurve)
            }
            
            labels <-  c(10000, 1E5, 1E6, 10E6)
            breaks <- log(labels - (10E3) + 1)
            
            ggplot() +
                geom_histogram(
                    data = lastPops,
                    aes(x = sklogpop, y = ..density..),
                    colour = "black",
                    alpha =  0.2,
                    binwidth = 0.75,
                    fill = "firebrick1"
                ) +
                geom_density(
                    data = lastPops,
                    aes(x = sklogpop, y = ..density..),
                    colour = "firebrick1",
                    size = 1,
                    linetype = "twodash"
                ) +
                geom_line(
                    data = xyDF,
                    aes(x = x, y = y),
                    colour = "cornflowerblue",
                    size = 1.5
                ) +
                scale_x_continuous(breaks = breaks, labels = labels) +
                facet_wrap( ~ sysYear, scales = "fixed", nrow = 2) +
                ggtitle("log(Populations) histograms\n((Χ₀ =  10E3))") +
                theme_bw() +
                theme(strip.text = element_text(size = 14)) +
                xlab("Population (last census)")
        })
        
        output$shapeComparison <- renderPlot({
            shapeComparison <- shapeComparisonPlot()
            print(shapeComparison)
        })
        
        output$histDl <- downloadHandler(
            filename = function() {
                paste('histograms-', Sys.Date(), '.pdf', sep = '')
            },
            content = function(file) {
                ggsave(file, plot = shapeComparisonPlot(), device = pdf)
            }
        )
        
        qqplotsComparisonPlot <- reactive({
            maxyear <- BRICS %>%
                group_by(system) %>%
                summarise(yearmax = max(year))
            
            lastPops <- BRICS %>%
                semi_join(maxyear, by = c("system",  "year" = "yearmax")) %>%
                filter(pop > 10E3,!is.na(pop)) %>%
                mutate(logpop = log(pop)) %>%
                mutate(sklogpop = log(pop - 10E3))
            lastPops$sysYear <-
                paste(lastPops$system, "\n(",  lastPops$year, ")", sep = "")
            
            xyDF <-
                data.frame(
                    system = character(),
                    x = numeric(),
                    y = numeric(),
                    stringsAsFactors = FALSE
                )
            countriesR2 <-
                data.frame(
                    sysYear = character(),
                    r2 = character(),
                    stringsAsFactors = FALSE
                )
            for (currentSysYear in unique(lastPops$sysYear)) {
                currentPops <- lastPops %>%
                    filter(sysYear == currentSysYear)
                
                qqDiff <- data.frame(
                    sysYear = currentSysYear,
                    as.data.frame(qqnorm(
                        y = currentPops$sklogpop,  plot.it = FALSE
                    )),
                    stringsAsFactors = FALSE
                )
                xyDF <- xyDF %>%
                    bind_rows(qqDiff)
                
                currentLM <- summary(lm(qqDiff$x ~ qqDiff$y))
                currentR2 <-
                    round(currentLM$r.squared, digits = 4) * 100
                countriesR2 <- countriesR2 %>%
                    bind_rows(data.frame(
                        sysYear = currentSysYear,
                        r2 = sprintf("R² = %s%%", currentR2)
                    ))
            }
            
            xyDF$x <- exp(xyDF$x)
            xyDF$y <- exp(xyDF$y)
            breaks <- c(1, 15000 , 90000, 990000, 9990000)
            labels <- c("10 000", "25 000", "100 000", "1E6", "10E6")
            
            minValue <- min(xyDF$x, xyDF$y, na.rm = TRUE)
            
            ggplot(data = xyDF, aes(x = x, y = y)) +
                geom_smooth(
                    method = "lm",
                    level = 0.5,
                    size = 1.5,
                    colour = "cornflowerblue"
                ) +
                geom_point(colour = "black",
                           alpha =  0.2,
                           fill = "firebrick1") +
                scale_x_log10(breaks = breaks, labels = labels) +
                scale_y_log10(breaks = breaks, labels = labels) +
                geom_text(data = countriesR2, aes_string(
                    x = minValue,
                    y = minValue,
                    label = "r2"
                )) +
                facet_wrap( ~ sysYear, scales = "fixed",nrow = 2) +
                ggtitle("Normal Q-Q plot\n(log(Populations (Χ₀ =  10E3)))") +
                theme_bw() +
                theme(strip.text = element_text(size = 14)) +
                xlab("Theoretical Population") +
                ylab("Observed Population")
            
        })
        
        output$qqplotsComparison <- renderPlot({
            qqplotsComparison <- qqplotsComparisonPlot()
            print(qqplotsComparison)
        })
        
        output$qqDl <- downloadHandler(
            filename = function() {
                paste('QQplot-', Sys.Date(), '.pdf', sep = '')
            },
            content = function(file) {
                ggsave(file, plot = qqplotsComparisonPlot(), device = pdf)
            }
        )
        
        output$normalityComparison <- renderTable({
            maxyear <- BRICS %>%
                group_by(system) %>%
                summarise(yearmax = max(year))
            
            lastPops <- BRICS %>%
                semi_join(maxyear, by = c("system",  "year" = "yearmax")) %>%
                filter(pop > 10E3,!is.na(pop)) %>%
                mutate(logpop = log(pop)) %>%
                mutate(sklogpop = log(pop - 10E3))
            
            resultDF <-
                data.frame(matrix(0, nrow = 9, ncol = length(unique(
                    lastPops$system
                ))), stringsAsFactors = FALSE)
            colnames(resultDF) <- unique(lastPops$system)
            row.names(resultDF) <-
                c(
                    "Year",
                    "Nb Cities",
                    "meanLog",
                    "sdLog",
                    "RelSd",
                    "p.value (Shapiro-Wilk)",
                    "p.value (Kolmogorov-Smirnoff)",
                    "skewness",
                    "kurtosis"
                )
            for (currentSystem in unique(lastPops$system)) {
                currentPops <- lastPops %>%
                    filter(system == currentSystem)
                
                year <- as.numeric(unique(currentPops$year))
                nbCities <- nrow(currentPops)
                meanLog <- mean(currentPops$sklogpop)
                sdLog <- sd(currentPops$sklogpop)
                relsd <- sdLog / meanLog
                skewnessLog <- skewness(currentPops$sklogpop)
                kurtosisLog <- kurtosis(currentPops$sklogpop)
                
                if (nbCities > 5000) {
                    SW.data <- sample(x = currentPops$sklogpop, size = 5000)
                } else {
                    SW.data <-  currentPops$sklogpop
                }
                SW.pvalue <-
                    suppressWarnings(shapiro.test(SW.data)$p.value)
                KStest <-
                    suppressWarnings(ks.test(currentPops$sklogpop, "pnorm", exact = FALSE))
                KS.pvalue <- KStest$p.value
                resultDF[, currentSystem] <-
                    c(
                        year,
                        nbCities,
                        meanLog,
                        sdLog,
                        relsd,
                        SW.pvalue,
                        KS.pvalue,
                        skewnessLog,
                        kurtosisLog
                    )
            }
            
            resultDF
        })
        
        sysZipfEvolutionPlot <- reactive({
            rankedData <- BRICS %>%
                filter(!is.na(pop), pop >= 10E3) %>%
                group_by(system, year) %>%
                mutate(rank = min_rank(-pop))
            
            rankedData <- na.omit(rankedData)
            
            ggplot(
                data = rankedData,
                aes(
                    x = rank,
                    y = pop,
                    colour = year,
                    group = year
                ),
                environment = environment()
            ) +
                geom_line(size = 0.4) +
                scale_colour_gradient(low = "skyblue3", high = "firebrick4") +
                scale_alpha(range = c(0.1, 1)) +
                scale_y_log10() +
                scale_x_log10() +
                facet_wrap( ~ system, scales = "fixed", ncol = 2) +
                labs(title = "Rank-Size evolution",
                     x = "Rank",
                     y = "Population") +
                theme_bw()
        })
        
        
        output$sysZipfEvolution <- renderPlot({
            sysZipfEvolution <- sysZipfEvolutionPlot()
            print(sysZipfEvolution)
        }, height = 800)
        
        output$zipfEvolDl <- downloadHandler(
            filename = function() {
                paste('zipfEvolplot-', Sys.Date(), '.pdf', sep = '')
            },
            content = function(file) {
                ggsave(file, plot = sysZipfEvolutionPlot(), device = pdf)
            }
        )
        
        sysZipfLastPlot <- reactive({
            maxyear <- BRICS %>%
                group_by(system) %>%
                summarise(yearmax = max(year))
            
            rankedLastPops <- BRICS %>%
                semi_join(maxyear, by = c("system",  "year" = "yearmax")) %>%
                filter(!is.na(pop), pop >= 10E3) %>%
                group_by(system) %>%
                mutate(rank = min_rank(-pop))
            
            ggplot(
                data = rankedLastPops,
                aes(
                    x = rank,
                    y = pop,
                    group = system,
                    colour = system
                ),
                environment = environment()
            ) +
                geom_line(size = 1) +
                scale_y_log10() +
                scale_x_log10() +
                #facet_wrap(~system, scales =  "free", ncol = 3) +
                labs(title = "Rank-Size evolution",
                     x = "Rank",
                     y = "Population") +
                theme_bw()
        })
        
        output$sysZipfLast <- renderPlot({
            sysZipfLast <- sysZipfLastPlot()
            print(sysZipfLast)
        })
        
        
        output$zipfLastDl <- downloadHandler(
            filename = function() {
                paste('zipfLastplot-', Sys.Date(), '.pdf', sep = '')
            },
            content = function(file) {
                ggsave(file, plot = sysZipfLastPlot(), device = pdf)
            }
        )
        
        output$relativesSharesApported <- renderUI({
            blob <- list()
            for (currSys in unique(BRICS$system)) {
                currentWideDF <-
                    BRICS %>% filter(system == currSys, pop > 10E3) %>% dplyr::select(ID, year, pop) %>% tidyr::spread(year, pop)
                resultWideDF <- currentWideDF
                resultWideDF[, ] <- NA
                for (currRow in 1:nrow(currentWideDF)) {
                    for (currCol in 3:ncol(currentWideDF)) {
                        if (is.na(currentWideDF[currRow, currCol - 1]) &&
                            !is.na(currentWideDF[currRow, currCol])) {
                            resultWideDF[currRow, currCol] <-
                                currentWideDF[currRow, currCol]
                        }
                    }
                }
                newPops <- apply(X = resultWideDF[, -1], MARGIN = 2,
                                 FUN = function(x) { sum(x, na.rm = TRUE)}
                                 )
                sumPops <- apply(X = currentWideDF[, -1], MARGIN = 2,
                                 FUN = function(x) { sum(x, na.rm = TRUE)}
                                 )
                newCities <- apply(X = resultWideDF[, -1], MARGIN = 2,
                                   FUN = function(x) { length(na.omit(x))}
                                   )
                sumCities <- apply(X = currentWideDF[, -1], MARGIN = 2,
                                   FUN = function(x) {length(na.omit(x))}
                                   )
                
                sharePops <- newPops / sumPops
                shareCities <- newCities / sumCities
                
                grouped_summary <- BRICS %>%
                    filter(!is.na(pop), system == currSys) %>%
                    group_by(year) %>%
                    summarise(nbCities = n(),
                              totalPop = sum(pop, na.rm = TRUE))
                
                
                #grouped_summary
                df <- as.data.frame(t(grouped_summary))
                colnames(df) <- df[1, ]
                
                growthratetable <- df[1:ncol(df) - 1]
                # Creation des noms de périodes
                for (i in 1:ncol(growthratetable))
                {
                    t0_name <- colnames(df[i])
                    t1_name <- colnames(df[i + 1])
                    myname <-
                        as.character(paste(t0_name, t1_name, sep = "-"))
                    colnames(growthratetable)[i] <- myname
                }
                growthratetable <- growthratetable[-1, ]
                baseDF <- grouped_summary
                for (i in (2:nrow(baseDF))) {
                    growthratetable[1, i - 1] <- newCities[i]
                    growthratetable[2, i - 1] <- shareCities[i]
                    growthratetable[3, i - 1] <- newPops[i]
                    growthratetable[4, i - 1] <- sharePops[i]
                }
                growthratetable
                rownames(growthratetable) <-
                    c("New Cities",  "% new cities", "New Pop", "% new pop")
                
                
                blob[[currSys]] <-
                    # save table into slot in created list
                    # print table as HTML with additional formatting options
                    print(
                        xtable(growthratetable, caption = paste("System:", currSys)),
                        type = "html",
                        html.table.attributes = 'class="data table table-bordered table-condensed"',
                        caption.placement = "top",
                        print.results = FALSE
                    )
            }
            #browser()
            return(div(HTML(unlist(blob)),class = "shiny-html-output"))
        })
        
        output$populationmaps <- renderPlot({
            maxyear <- BRICS %>%
                group_by(system) %>%
                summarise(yearmax = max(year))
            
            lastPops <- BRICS %>%
                semi_join(maxyear, by = c("system",  "year" = "yearmax")) %>%
                filter(pop > 10E3,!is.na(pop))
            
            
            
            maxPop <- max(lastPops$pop)
            
            currentCountry <- input$countryMap 
            
            currentPops <- as.data.frame(lastPops %>%filter(system == currentCountry),
                              stringsAsFactors = FALSE)
            
            
            mapString <- currentCountry
            
            coordinates(currentPops) <- ~ Long + Lat
            proj4string(currentPops) <- CRS("+init=epsg:4326")
           
            if (currentCountry == "Former USSR") {
                mapString <- "Georgia|Armenia|Azerbaijan|Belarus|Estonia|Kazakhstan|Kyrgyzstan|Latvia|Lithuania|Moldavia|Russia|Tajikistan|Turkmenistan|Ukraine|Uzbekistan"
            } else if (currentCountry == "USA") {
                mapString <- "USA(?!:(Alaska|Hawaii))"
            } else if (currentCountry == "Europe") {
                mapString <- "France|Austria|Belgium|Bulgaria|Switzerland|Cyprus|Czech|Germany|Denmark|Estonia|Spain|Finland|Greece|Croatia|Hungary|^Ireland|Italy|Lithuania|Luxembourg|Latvia|Malta|Netherlands|Poland|Portugal|Romania|Sweden|Slovenia|Slovakia|UK:"
            } else if (currentCountry == "South Africa"){
                mapString <- "South Africa(?!:)"
            } else if (currentCountry == "China (Historical)"){
                mapString <- "China"
            }
            
            map(regions= mapString)
            
            propSymbolsLayer(
                spdf = currentPops, df = currentPops@data, var = "pop",
                k = input$symbolSize, fixmax = maxPop,
                symbols = "circle", border = "white",
                lwd = 0.1, legend.pos = "topleft", legend.title.txt = "Total population"
            )

            layoutLayer(
                title = sprintf("Cities in %s", currentCountry),
                author = "", sources = "", scale = NULL,
                frame = TRUE, col = "#688994"
            )
            
        }, height = 800)
        
        output$mapDl <- downloadHandler(
            filename = function() {
                paste(input$countryMap, "_map", '.pdf', sep = '')
            },
            content = function(file) {
                pdf(file = file,  paper = "a4r")
                maxyear <- BRICS %>%
                    group_by(system) %>%
                    summarise(yearmax = max(year))
                
                lastPops <- BRICS %>%
                    semi_join(maxyear, by = c("system",  "year" = "yearmax")) %>%
                    filter(pop > 10E3,!is.na(pop))
                
                
                
                maxPop <- max(lastPops$pop)
                
                currentCountry <- input$countryMap 
                
                currentPops <- as.data.frame(lastPops %>%filter(system == currentCountry),
                                             stringsAsFactors = FALSE)
                
                
                mapString <- currentCountry
                
                coordinates(currentPops) <- ~ Long + Lat
                proj4string(currentPops) <- CRS("+init=epsg:4326")
                
                if (currentCountry == "Former USSR") {
                    mapString <- "Georgia|Armenia|Azerbaijan|Belarus|Estonia|Kazakhstan|Kyrgyzstan|Latvia|Lithuania|Moldavia|Russia|Tajikistan|Turkmenistan|Ukraine|Uzbekistan"
                } else if (currentCountry == "USA") {
                    mapString <- "USA(?!:(Alaska|Hawaii))"
                } else if (currentCountry == "Europe") {
                    mapString <- "France|Austria|Belgium|Bulgaria|Switzerland|Cyprus|Czech|Germany|Denmark|Estonia|Spain|Finland|Greece|Croatia|Hungary|^Ireland|Italy|Lithuania|Luxembourg|Latvia|Malta|Netherlands|Poland|Portugal|Romania|Sweden|Slovenia|Slovakia|UK:"
                } else if (currentCountry == "South Africa"){
                    mapString <- "South Africa(?!:)"
                }
                
                map(regions= mapString)
                
                propSymbolsLayer(
                    spdf = currentPops, df = currentPops@data, var = "pop",
                    k = input$symbolSize, fixmax = maxPop,
                    symbols = "circle", border = "white",
                    lwd = 0.1, legend.pos = "topleft", legend.title.txt = "Total population"
                )
                
                layoutLayer(
                    title = sprintf("Cities in %s", currentCountry),
                    author = "", sources = "", scale = NULL,
                    frame = TRUE, col = "#688994"
                )
                dev.off()
            }
        )
        
        updateInputs <- function(session, columns, realColumns) {
            updateSelectInput(
                session = session,
                inputId = "timeColumnSelected",
                choices = realColumns,
                selected = realColumns
            )
            updateSelectInput(
                session = session,
                inputId = "dateClasses",
                choices = realColumns,
                selected = realColumns[length(realColumns)]
            )
            updateSelectInput(
                session = session,
                inputId = "dateZipf",
                choices = realColumns,
                selected = realColumns[length(realColumns)]
            )
            updateSelectInput(
                session = session,
                inputId = "dateLogNormal",
                choices = realColumns,
                selected = realColumns[length(realColumns)]
            )
            updateSelectInput(
                session = session,
                inputId = "dateInitial",
                choices = realColumns,
                selected = realColumns[length(realColumns) -  1]
            )
            updateSelectInput(
                session = session,
                inputId = "dateFinal",
                choices = realColumns,
                selected = realColumns[length(realColumns)]
            )
        }
        
        resetValues <- function() {
            dataValues <- reactiveValues(
                rawDF = NULL,
                filtredDF = NULL,
                calcDF = NULL,
                growthTable = NULL,
                rawGrowthTable = NULL,
                rawGrowthTableSizeInit = NULL,
                lastCensusesTable  = NULL
            )
            
            computedValues <- reactiveValues(
                simData = NULL,
                simResults = NULL,
                simMeans = NULL,
                simSDs = NULL,
                meanRanks = NULL,
                simRanks = NULL,
                obsRanks = NULL
            )
            
            analysisValues <- reactiveValues(
                zipfTable = NULL,
                transitionMatrix = NULL,
                logNormalTable = NULL,
                fittedLogNormalTable  = NULL,
                zipfResTable = NULL,
                corGrowthSize = NULL,
                corTemporal = NULL
            )
            
            updateSelectInput(
                session = session,
                inputId = "timeColumnSelected",
                choices = NA,
                selected = ""
            )
            updateSelectInput(
                session = session,
                inputId = "dateClasses",
                choices = NA,
                selected = ""
            )
            updateSelectInput(
                session = session,
                inputId = "dateZipf",
                choices = NA,
                selected = ""
            )
            updateSelectInput(
                session = session,
                inputId = "dateLogNormal",
                choices = NA,
                selected = ""
            )
            updateSelectInput(
                session = session,
                inputId = "dateInitial",
                choices = NA,
                selected = ""
            )
            updateSelectInput(
                session = session,
                inputId = "dateFinal",
                choices = NA,
                selected = ""
            )
        }
     
})
    