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
    growth_table <- compute_growthtable(df=df)
    simList <- mclapply(X=c(1:reps),function (x) run_replication(obs_data=df, growthtable=growth_table ), mc.cores=24)
    L <- length(simList)
    RC <- dim(simList[[1]])
    simArray <- array(unlist(simList), dim=c(RC[1], RC[2], L))
    print('ok6')
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
            my_replication_matrix[j,i+1] <-  currentPop * (1 + normalGrowthRate/100)
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