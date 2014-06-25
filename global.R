require(parallel)


############ COMPUTE_GROWTHTABLE ############
compute_growthtable <- function (df)
{
    growthratetable <- df[1:ncol(df)-1]
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
    
    # On le remplit avec les taux de croissance
    for (rownb in 1:nrow(growthratetable_matrix))
    {
        for (colnb in 1:ncol(growthratetable_matrix))
        {
            growthratetable_matrix[rownb,colnb] <-  ( (df_matrix[rownb, colnb + 1] - df_matrix[rownb, colnb])/df_matrix[rownb, colnb]) * 100
        }
    }
    growthratetable[] <- growthratetable_matrix[]
    growthparameters <- growthratetable[1:2,]
    rownames(growthparameters) <- c("Mean Growth (%)", "Growth StDev (%)")
    growthparameters[1,] <- apply(X=growthratetable, MARGIN=2, FUN=mean) # Calcul de la moyenne
    growthparameters[2,] <- apply(X=growthratetable, MARGIN=2, FUN=sd) # Calcul de l'écart-type
    return(growthparameters)
}

############ RUN_SIMULATION ############
run_simulation <- function (df, reps)
{
    #growth_table1 <- compute_growthtable(df=df)
    yearlyGT <- compute_yearly_growth_table(df)
    growth_table <- expand_growth_table(yearlyGT)
    
    simList <- mclapply(X=c(1:reps),function (x) run_replication(obs_data=df, growthtable=growth_table ), mc.cores=24)
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
    initial_data <- obs_data[1]
    timesteps <- length(growthtable)
    my_replication <- obs_data
    my_replication[,2:timesteps+1] <- NA
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
            my_replication_matrix[j,i+1] <-  ifelse(newPop > 0, newPop, 1)
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


compute_yearly_growth_table <- function(df)
{
    growthratetable <- df[1:ncol(df)-1]
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
    
    # On le remplit avec les taux de croissance
    for (rownb in 1:nrow(growthratetable_matrix))
    {
        for (colnb in 1:ncol(growthratetable_matrix))
        {
            diffDate <- as.numeric(colnames(df_matrix)[colnb + 1]) - as.numeric(colnames(df_matrix)[colnb])
            
            firstPop <- df_matrix[rownb, colnb]
            lastPop <- df_matrix[rownb, colnb + 1]
            growthratetable_matrix[rownb, colnb] <- ((lastPop / firstPop)^(1/diffDate) - 1) * 100
        }
    }
    growthratetable[] <- growthratetable_matrix[]
    #View(growthratetable[1000:nrow(growthratetable), ])
    growthparameters <- growthratetable[1:2,]
    rownames(growthparameters) <- c("Annual Mean Growth (%)", "Annual Growth StDev (%)")
    growthparameters[1,] <- apply(X=growthratetable, MARGIN=2, FUN=function(x){return(mean(x, na.rm=TRUE))}) # Calcul de la moyenne
    growthparameters[2,] <- apply(X=growthratetable, MARGIN=2, FUN=function(x){ return(sd(x, na.rm=TRUE))}) # Calcul de l'écart-type
    return(growthparameters)
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