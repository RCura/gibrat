
############ COMPUTE_GROWTHTABLE ############

compute_yearly_growth_table <- function(df)
{
    growthratetable <- df[1:ncol(df)-1]
    initSizeTable <- df[1:ncol(df)-1]
    # Creation des noms de périodes
    for (i in 1:ncol(growthratetable))
    {
        t0_name <- colnames(df[i])
        t1_name <- colnames(df[i+1])
        myname <- as.character(paste(t0_name, t1_name, sep="-"))
        colnames(growthratetable)[i] <- myname
    }
    growthratetable[,] <- NA
    growthratetable_matrix <- as.matrix(growthratetable)
    df_matrix <- as.matrix(df)
    initSizeTable[,] <-  NA
    initSizeTable_matrix <-  as.matrix(initSizeTable)
    # On le remplit avec les taux de croissance
    for (rownb in 1:nrow(growthratetable_matrix))
    {
        for (colnb in 1:ncol(growthratetable_matrix))
        {
            diffDate <- as.numeric(colnames(df_matrix)[colnb + 1]) - as.numeric(colnames(df_matrix)[colnb])
            
            firstPop <- df_matrix[rownb, colnb]
            lastPop <- df_matrix[rownb, colnb + 1]
            growthratetable_matrix[rownb, colnb] <- ((lastPop / firstPop)^(1/diffDate) - 1) * 100
            initSizeTable_matrix[rownb, colnb] <-  firstPop
        }
    }
    growthratetable[] <- growthratetable_matrix[]
    initSizeTable[] <-  initSizeTable_matrix[]
    #View(growthratetable[1000:nrow(growthratetable), ])
    growthparameters <- growthratetable[1:2,]
    rownames(growthparameters) <- c("Annual Mean Growth (%)", "Annual Growth StDev (%)")
    growthparameters[1,] <- apply(X=growthratetable, MARGIN=2, FUN=function(x){return(mean(x, na.rm=TRUE))}) # Calcul de la moyenne
    growthparameters[2,] <- apply(X=growthratetable, MARGIN=2, FUN=function(x){ return(sd(x, na.rm=TRUE))}) # Calcul de l'écart-type
    return(list(fullDF  =  growthratetable, growthRateSizeInitial =  initSizeTable,  Summary  = growthparameters))
}

expand_growth_table <- function(growthTable){
    periodNames <- colnames(growthTable)
    indicatorNames <- row.names(growthTable)
    tGrowthTable <- cbind.data.frame(periodNames,t(growthTable), stringsAsFactors=FALSE)
    colnames(tGrowthTable) <- c("Period", indicatorNames)
    tGrowthTable$FROM <- as.numeric(unlist(lapply(tGrowthTable$Period, FUN=function(x){abc <- strsplit(x, split="-") ; return(abc[[1]][[1]])})))
    tGrowthTable$TO <- as.numeric(unlist(lapply(tGrowthTable$Period, FUN=function(x){abc <- strsplit(x, split="-") ; return(abc[[1]][[2]])})))
    ### tGrowthTable is great there
    
    expandedGrowthTable <- tGrowthTable[rep(1:nrow(tGrowthTable), (tGrowthTable$TO - tGrowthTable$FROM)),]
    expandedGrowthTable$FROM <- (expandedGrowthTable$FROM[1]:(expandedGrowthTable$TO[nrow(expandedGrowthTable)] - 1))
    expandedGrowthTable$TO <- expandedGrowthTable$FROM + 1
    expandedGrowthTable$Period <- paste(expandedGrowthTable$FROM, expandedGrowthTable$TO, sep="-")
    row.names(expandedGrowthTable) <- expandedGrowthTable$Period
    expandedGrowthTable <- expandedGrowthTable[,-c(1,4,5)]
    return(as.data.frame(t(expandedGrowthTable)))
}

############ RUN_SIMULATION ############
run_simulation <- function (df, reps)
{
    yearlyGT <- compute_yearly_growth_table(df)$Summary
    growth_table <- expand_growth_table(yearlyGT)
    
    simList <- mclapply(X=c(1:reps),function (x) run_replication(obs_data=df, growthtable=growth_table ), mc.cores=12)
    L <- length(simList)
    RC <- dim(simList[[1]])
    simArray <- array(unlist(simList), dim=c(RC[1], RC[2], L))
    firstDate <- colnames(df)[1]
    lastDate <- colnames(df)[length(colnames(df))]
    
    dimnames(simArray) <- list(rownames(df), (firstDate:lastDate) , paste("Sim", 1:reps, sep=""))
    return(simArray)
}

############ RUN_REPLICATION ############
run_replication <- function (obs_data, growthtable)
{
    initial_data <- obs_data[,1]
    timesteps <- length(growthtable)
    my_replication <- obs_data
    firstDate <- min(as.numeric(colnames(obs_data)))
    lastDate <- max(as.numeric(colnames(obs_data)))
    nbDates <- lastDate - firstDate + 1
    emptyDF <- as.data.frame(setNames(replicate(nbDates,numeric(0), simplify = F), 1:nbDates),
                             check.names = FALSE)
    colnames(emptyDF) <- firstDate:lastDate 
    emptyDF <- emptyDF[, !(names(emptyDF) %in% colnames(obs_data))]
    emptyDF[1:nrow(obs_data),] <- NA
    emptyDF <- cbind(emptyDF, obs_data)
    
    my_replication <- emptyDF[, order(colnames(emptyDF))]
    cities_nb <- nrow(my_replication)
    growthmean <- as.double(growthtable[1,])
    growthsd <- as.double(growthtable[2,])
    my_replication_matrix <- as.matrix(my_replication)
    for (i in 1:timesteps)
    {
        currentGrowthMean <- growthmean[i]
        currentGrowthSd <- growthsd[i]
        for (j in 1:cities_nb)
        {
            currentPop <- my_replication_matrix[j,i]
            normalGrowthRate <- rnorm(1, mean=currentGrowthMean, sd=currentGrowthSd)
            
            newPop <- currentPop * (1 + normalGrowthRate/100)
            
            ## If it's the last step, do not keep values for comparison
            if (i == timesteps){
                my_replication_matrix[j, i+1] <- ifelse(!is.na(newPop),ifelse(newPop > 0, newPop, 1), NA)
            } else {
                if (!is.na(newPop)) {
                    my_replication_matrix[j,i+1] <-  ifelse(newPop > 0, newPop, 1)                     }
            }
        }
    }
    my_replication[] <- my_replication_matrix[]
    return(my_replication)
}

############ CREATE_MEANRANK_TABLES ############
create_rank_tables <- function (obsdata, simMean)
{
    obsRank <- simRank <- obsdata
    obsRank_matrix <- simRank_matrix <- as.matrix(obsRank)
    obsRank_matrix[] <- simRank_matrix[] <- NA
    obsdata <- as.matrix(obsdata)
    
    for (i in 1:ncol(obsRank_matrix))
    {
        simRank_matrix[,i] <- rank(-simMean[,i])
        obsRank_matrix[,i] <- rank(-obsdata[,i])
    }
    simRank[] <- simRank_matrix[]
    obsRank[] <- obsRank_matrix[]
    return(list(simRank, obsRank))
}

plotRankSize <- function(baseDF){
    baseDF$ID <- row.names(baseDF)
    meltedDF <- melt(data=baseDF,id.vars="ID")
    
    timeValues <- unique(meltedDF$variable)
    for (currentTime in timeValues){
        myValues <- meltedDF[meltedDF$variable == currentTime, 'value']
        meltedDF[meltedDF$variable == currentTime,'rank'] <- rank(x=-myValues)
    }
    ranksizePlot <- ggplot(data=meltedDF, environment = environment()) + 
        geom_line(aes(x=rank, y=value, group=variable, colour=as.numeric(levels(variable)[variable])), size=1) +
        scale_colour_gradient(low="skyblue3", high="firebrick4", "Date") +
        scale_alpha(range=c(0.1,1)) +
        scale_y_log10() +
        scale_x_log10() +
        labs(title = "Rank-Size evolution",
             x = "Rank",
             y = "Population") +
        theme_bw()
    
    return(ranksizePlot)
}

#https://users.dimi.uniud.it/~massimo.franceschet/R/fit.html
lognormal <- function(d, limit=2500) {
    
    # load MASS package to use fitdistr
    # mle = fitdistr(d, "lognormal");
    # meanlog = mle$estimate["meanlog"];
    # sdlog = mle$estimate["sdlog"];
    
    # MLE for lognormal distribution
    meanlog = mean(log(d));
    sdlog = sd(log(d));
    
    # compute KS statistic
    t = ks.test(d, "plnorm", meanlog = meanlog, sdlog = sdlog);
    
    # compute p-value
    count = 0;
    for (i in 1:limit) {
        syn = rlnorm(length(d), meanlog = meanlog, sdlog = sdlog);
        meanlog2 = mean(log(syn));
        sdlog2 = sd(log(syn));
        t2 = ks.test(syn, "plnorm", meanlog = meanlog2, sdlog = sdlog2);
        if(t2$stat >= t$stat) {count = count + 1};
    }
    
    return(list(meanlog=meanlog, sdlog=sdlog, stat = t$stat, p = count/limit, KSp = t$p));
    
}

lognormal10 <- function(d, limit=2500) {
    
    # load MASS package to use fitdistr
    # mle = fitdistr(d, "lognormal");
    # meanlog = mle$estimate["meanlog"];
    # sdlog = mle$estimate["sdlog"];
    
    # MLE for lognormal distribution
    meanlog = mean(log10(d));
    sdlog = sd(log10(d));
    
    # compute KS statistic
    t = ks.test(d, "plnorm", meanlog = meanlog, sdlog = sdlog);
    
    # compute p-value
    count = 0;
    for (i in 1:limit) {
        syn = rlnorm(length(d), meanlog = meanlog, sdlog = sdlog);
        meanlog2 = mean(log10(syn));
        sdlog2 = sd(log10(syn));
        t2 = ks.test(syn, "plnorm", meanlog = meanlog2, sdlog = sdlog2);
        if(t2$stat >= t$stat) {count = count + 1};
    }
    
    return(list(meanlog=meanlog, sdlog=sdlog, stat = t$stat, p = count/limit, KSp = t$p));
    
}