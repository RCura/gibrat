#####  LOGNORMALITY  #####

myData  <-  Russia$`2010`
meanLog <- mean(log(myData))
sdLog <- sd(log(myData))
#sdLog <- log(sd(myData))
hist(myData, col="aquamarine3", breaks = 100, freq = FALSE)
lines(density(myData))
lines(dlnorm(myData, meanlog = meanLog, sdlog = sdLog), lwd=3, col="blue")

r <- hist(myData, breaks = 100, freq = FALSE)
plot(r$breaks[-1], r$counts, log='xy', type='h')


threshold <-  5E6
currentData <-  myData[myData  < threshold]
hist(currentData, prob = TRUE, breaks = 100)
lines(dlnorm(seq(0,threshold,by = 1), meanlog= mean(log(currentData)), sdlog = sd(log(currentData))), type = "l",col = "blue", lwd  =  2)

baseData <- myData - 10E3
baseData <- baseData[baseData > 0]
ticks <- log(c(10002,15E3,25E3,50E3,100E3, 1E6, 10E6, 20E6)  - 10E3)
labels <-  format(c(10000,15E3,25E3,50E3,100E3, 1E6, 10E6, 20E6), scientific = TRUE, big.mark = ",", trim=TRUE, justify="right", digits = 4)
hist(log(baseData), breaks = 100, prob = TRUE, xaxt  ="n", xlab="Population (log-scale)", ylab="Density")
axis(side = 1, at=ticks, labels=labels, las=1)
lines(dnorm(x = 0:max(log(baseData)), mean = mean(log(baseData)), sd = sd(log(baseData))), type = "l", col = "blue", lwd = 2)

blob <- dnorm(x = 0:max(log(baseData)), mean = mean(log(baseData)), sd = sd(log(baseData)))
qqnorm(x  =  blob, y = log(baseData), xaxt= "n", yaxt  = "n", type="p", pch=19, cex=1, col=rgb(0, 0, 1, 0.07))
qqline(log(baseData))
axis(side = 1, at=ticks, labels=labels, las=1, cex.axis=0.6)
axis(side = 2, at=ticks, labels=labels, las=1, cex.axis=0.6)

baseData2 <- data.frame(x = baseData, y = baseData + 10E3, z = log(baseData), w = log(baseData + 10E3))
fit1 <- fitdistr(baseData2$x, densfun = "lognormal")
fit2 <- fitdistr(baseData2$z, densfun = "normal")
ggplot(data=baseData2,  aes(x=x)) + geom_histogram(aes(y = ..density..), binwidth=10E3) +
    stat_function(fun = dnorm, size=1, color='blue', args = list(fit1$estimate[1], fit1$estimate[2]))

library(poweRlaw)
library(staTools)

pops <- Russia
pops$datepop  <-  Russia$`2010`
Populations <- as.data.frame(sort(pops$datepop,decreasing = TRUE))
colnames(Populations) <- c("Pop")
# Populations
ln_m <- dislnorm$new(Populations$Pop)
print(ln_m)
system.time(est_ln <- estimate_xmin(ln_m))

pl_m  <- displo(x = Populations$Pop, summary = TRUE)
est_plo  <-  getXmin2(o = pl_m)
system.time(est_ln2 <- getXmin(o = blob))
system.time(est_ln3 <- getXmin2(blob))
print(est_ln)
print(est_ln2)
print(est_ln3)

ln_m$setXmin(est_ln)
print(ln_m)

ln_estim = data.frame(matrix(ncol = 3, nrow = 1))
ln_estim[1,1] <- ln_m$pars[[1]]
ln_estim[1,2] <- ln_m$pars[[2]]
ln_estim[1,3] <- ln_m$xmin
colnames(ln_estim) <- c("Mean", "Standard Deviation", "X min")
print(ln_estim)
analysisValues$fittedLogNormalTable  <-  ln_estim


getXmin3 <- function(o, g = 10E3, c = 10, k = 5, xmax = 1E6){
    est = list()
    x = o$x
    N = o$nx
    g = g - (g * (100 - c)/100)
    xmins = o$ux[o$ux <= xmax]
    START = xmins[which.min(abs(xmins - g))]
    if (START > g) {
        START = xmins[which.min(abs(xmins - g))]
    }
    if (length(START) == 0) {
        START = 1
    }
    xmins = xmins[which(xmins == START):length(xmins)]
    L = length(xmins)
    KS = numeric()
    alpha = numeric()
    xu = sort(x)
    len_xu = length(xu)
    for (i in 1:L) {
        n = length(xu[xu >= xmins[i]])
        q = xu[(N - n + 1):len_xu]
        q = q[q <= xmax]
        S = pmf(q)$y
        alpha = c(alpha, 1 + length(q)/sum(log(q/(xmins[i] - 
                                                      0.5))))
        P = ddispl(unique(q), xmin = xmins[i], alpha = alpha[i])
        KS = c(KS, max(abs(P - S)))
    }
    est$xmin = xmins[which.min(KS)]
    est$alpha = alpha[which.min(KS)]
    o$xmin = xmins[which.min(KS)]
    o$alpha = alpha[which.min(KS)]
    o$sigma = (alpha[which.min(KS)] - 1)/sqrt(N)
    return(est)
}


pops <- China
pops$datepop  <-  pops$`2010`
Populations <- as.data.frame(sort(pops$datepop,decreasing = TRUE))
colnames(Populations) <- c("Pop")

pl_m  <- displo(x = Populations$Pop, summary = TRUE)
getXmin3(pl_m)



#####  GIBRAT SIM  #####

testDF <- data.frame(`1900` = c(10,20,30,NA,50, NA),check.rows = FALSE, check.names = FALSE, stringsAsFactors = FALSE)
testDF$`1905`  <-  c(10, 20, 30, 10, 50, NA)
testDF$`1908`  <-  c(NA, 25, 35, 20, 80, NA)
testDF$`1911`  <-  c(20, 20, NA, 10, 100, NA)
testDF[6,] <- c(NA, NA, NA, 20)
testDF

source(file = 'global.R')

df <- testDF

###
#df <- SouthAfrica[,5:10]

###
reps <- 1

    yearlyGT <- compute_yearly_growth_table(df)$Summary
    growth_table <- expand_growth_table(yearlyGT)
    
    blob <- run_replication(df, growth_table)
    blob
    
    
    obs_data <-  df
    growthtable <- growth_table
        initial_data <- obs_data[1]
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
        View(my_replication_matrix)
        for (i in 1:timesteps)
        {
            currentGrowthMean <- growthmean[i]
            currentGrowthSd <- growthsd[i]
            for (j in 1:cities_nb)
            {
                currentPop <- my_replication_matrix[j,i]
                normalGrowthRate <- rnorm(1, mean=currentGrowthMean, sd=currentGrowthSd)
                
                newPop <- currentPop * (1 + normalGrowthRate/100)
                
                if (i == timesteps){
                    my_replication_matrix[j, i+1] <- ifelse(!is.na(newPop),ifelse(newPop > 0, newPop, 1), NA)
                } else {
                    if (!is.na(newPop)) {
                        my_replication_matrix[j,i+1] <-  ifelse(newPop > 0, newPop, 1)                     }
                }
            }
        }
        my_replication[] <- my_replication_matrix[]
        View(my_replication)
    
    
    myList <- run_replication(obs_data = df, growthtable = growth_table)
    
    
    
    
    
    
    
    #simList <- mclapply(X=c(1:reps),function (x) run_replication(obs_data=df, growthtable=growth_table ), mc.cores=12)
    L <- length(simList)
    RC <- dim(simList[[1]])
    simArray <- array(unlist(simList), dim=c(RC[1], RC[2], L))
    firstDate <- colnames(df)[1]
    lastDate <- colnames(df)[length(colnames(df))]
    
    dimnames(simArray) <- list(rownames(df), (firstDate:lastDate) , paste("Sim", 1:reps, sep=""))
View(simArray[,,1])
     


#####  SYSTEM DB BUILDING  #####

library(tidyr)
library(dplyr)

Brazil$system <- "Brazil"
China$system <- "China"
France$system <- "France"
India$system <- "India"
Russia$system <- "FSU"
SouthAfrica$system <- "South Africa"
USA$system <- "USA"

BrazilLong <- Brazil %>%
    select(ID, Name, Lat, Long, system,`1960`:`2010`) %>%
    gather(year, pop,  `1960`:`2010`) %>%
    mutate(year=as.integer(as.character(year)))
glimpse(BrazilLong)

ChinaLong <- China %>%
    gather(year, pop,  `1964`:`2010`) %>%
    mutate(year=as.integer(as.character(year)))
glimpse(ChinaLong)

FranceLong <- France %>%
    select(ID, Name, Lat, Long, system, `1962`:`1999`) %>%
    gather(year, pop,  `1962`:`1999`) %>%
    mutate(year=as.integer(as.character(year)))
glimpse(FranceLong)

IndiaLong <- India %>%
    select(ID, Name, Lat, Long, system, `1961`:`2011`) %>%
    gather(year, pop,  `1961`:`2011`) %>%
    mutate(year=as.integer(as.character(year)))
glimpse(IndiaLong)

FSULong <- Russia %>%
    select(ID, Name, Lat, Long, system, `1959`:`2010`) %>%
    gather(year, pop,  `1959`:`2010`) %>%
    mutate(year=as.integer(as.character(year)))
glimpse(FSULong)

SouthAfricaLong <- SouthAfrica %>%
    select(ID, Name, Lat, Long, system, `1960`:`2001`) %>%
    gather(year, pop,  `1960`:`2001`) %>%
    mutate(year=as.integer(as.character(year)))
glimpse(SouthAfricaLong)

Europe$system <- "Europe"

Europe <- subset(Europe, select = -Country)

EuropeLong <- Europe %>%
    gather(year, pop,  `1961`:`2011`) %>%
    mutate(year=as.integer(as.character(year)))
glimpse(EuropeLong)

USALong <- USA %>%
    select(ID, Name, Lat, Long, system, `1960`:`2010`) %>%
    gather(year, pop,  `1960`:`2010`) %>%
    mutate(year=as.integer(as.character(year)))
glimpse(USALong)

BRICS <- BrazilLong %>%
    bind_rows(ChinaLong) %>%
    bind_rows(IndiaLong) %>%
    bind_rows(FSULong) %>%
    bind_rows(SouthAfricaLong)%>%
    bind_rows(USALong)

BRICS <- BRICS %>%
    bind_rows(Europe)

rm(ChinaLong)

BRICS <- BRICS %>%
    filter(system != 'France')

View(BRICS)
 rm(list= c("BrazilLong", "ChinaLong", "FranceLong", "FSULong", "IndiaLong", "SouthAfricaLong", "USALong"))
 
 #####  SYSTEM COMPARISON  #####

maxyear <- BRICS %>%
    group_by(system) %>%
    summarise(yearmax = max(year))

lastPops <- BRICS %>%
    semi_join(maxyear, by= c("system",  "year" = "yearmax")) %>%
    filter(pop > 10E3, !is.na(pop)) %>%
    mutate(logpop = log(pop)) %>%
    mutate(sklogpop = log(pop - 10E3))

xyDF <- data.frame(system = character(), x=numeric(), y=numeric(), stringsAsFactors=FALSE) 

for (currentSystem in unique(lastPops$system)){
    currentPops <- lastPops %>%
        filter(system == currentSystem)

    qqDiff <- data.frame(system = currentSystem,
                          as.data.frame(qqnorm(y = currentPops$sklogpop,  plot.it = FALSE)),
                          stringsAsFactors = FALSE)
    xyDF <- xyDF %>%
        bind_rows(qqDiff)
}

labels <-  c(10000,1E5,1E6,10E6)
breaks <- log(labels)

ggplot(data = xyDF, aes(x=x, y=y)) +
    geom_smooth(method="lm", level=0.5, size=1.5, colour="cornflowerblue") +
    geom_point(colour = "black",  alpha =  0.2,  fill = "firebrick1") +
    scale_x_continuous(breaks=breaks, labels=labels) +
    scale_y_continuous(breaks=breaks, labels=labels) +
    facet_wrap(~system, scales = "fixed", ncol = 7) + 
    ggtitle("Normal Q-Q plot\n(log(Populations) cut at 10E3)") +
    theme_bw() +
    theme(strip.text = element_text(size=14)) +
    xlab("Theoretical Population") +
    ylab("Observed Population")

lastSA <- SouthAfrica$`2001`

    qqnorm(y = log(lastSA), xaxt= "n", yaxt  = "n",
           type="p", pch=19, cex=1,
           col=rgb(0, 0, 1, 0.1))
    qqline(log(lastSA))
    axis(side = 1, at=log(labels), labels=labels, las=1, cex.axis=0.6)
    axis(side = 2, at=log(labels), labels=labels, las=1, cex.axis=0.6)
    

geom_density(data=lastPops,  aes(x=logpop, y=..density..), colour = "firebrick1", size=1, linetype = "twodash") +
    geom_line(data=xyDF, aes(x = x, y = y), colour="cornflowerblue", size=1.5) +
    scale_x_continuous(breaks=breaks, labels=labels) +
    facet_wrap(~ system, scales = "fixed", ncol = 7) +
    ggtitle("Populations histograms") +
    theme_bw() +
    theme(strip.text = element_text(size=14)) +
    xlab("Population (last census)")




    maxyear <- BRICS %>%
        group_by(system) %>%
        summarise(yearmax = max(year))
    
    lastPops <- BRICS %>%
        semi_join(maxyear, by= c("system",  "year" = "yearmax")) %>%
        filter(pop > 10E3, !is.na(pop)) %>%
        mutate(logpop = log(pop)) %>%
        mutate(sklogpop = log(pop - 10E3))
    
    myLogNorm <- function(x, mean, sd){ dnorm(x, mean = mean, sd = sd)}
    
    minX <- min(lastPops$sklogpop)
    maxX <- max(lastPops$sklogpop)
    
    xyDF <- data.frame(system = character(), x=numeric(), y=numeric(), stringsAsFactors=FALSE) 
    
    for (currentSystem in unique(lastPops$system)){
        currentPops <- lastPops %>%
            filter(system == currentSystem)
        
        meanLog <- mean(currentPops$sklogpop, na.rm = TRUE)
        sdLog <- sd(currentPops$sklogpop,na.rm = TRUE)
        
        myCurve <- data.frame(system = currentSystem,
                              as.data.frame(curve(expr = myLogNorm(x, meanLog, sdLog),
                                                  xlim=c(minX,maxX))),
                              stringsAsFactors = FALSE)
        xyDF <- xyDF %>%
            bind_rows(myCurve)
    }
    
    labels <-  c(10000,1E5, 1E6, 10E6)
    breaks <- log(labels - (10E3) + 1)
    
    library(dplyr)
    library(ggplot2)
    
    maxyear <- BRICS %>%
        group_by(system) %>%
        summarise(yearmax = max(year))
    
    lastPops <- BRICS %>%
        semi_join(maxyear, by= c("system",  "year" = "yearmax")) %>%
        filter(pop > 10E3, !is.na(pop)) %>%
        mutate(logpop = log(pop)) %>%
        mutate(sklogpop = log(pop - 10E3))
    
    resultDF <- data.frame(matrix(0, nrow = 6, ncol = length(unique(lastPops$system))), stringsAsFactors = FALSE)
    colnames(resultDF) <- unique(lastPops$system)
    row.names(resultDF) <- c("Year", "Nb Cities", "meanLog",  "sdLog", "p.value (Shapiro-Wilk)", "p.value (Kolmogorov-Smirnoff)")
    for (currentSystem in unique(lastPops$system)){
        currentPops <- lastPops %>%
            filter(system == currentSystem)
        
        year <- as.numeric(unique(currentPops$year))
        nbCities <- nrow(currentPops)
        meanLog <- mean(currentPops$sklogpop)
        sdLog <- sd(currentPops$sklogpop)
        
        if (nbCities > 5000){
            SW.data <- sample(x = currentPops$sklogpop, size = 5000)
        } else {
            SW.data <-  currentPops$sklogpop
        }
        SW.pvalue <- shapiro.test(SW.data)$p.value
        KStest <- ks.test(currentPops$sklogpop, rnorm(5000, mean = meanLog, sd = sdLog))
        KS.pvalue <- KStest$p.value
        resultDF[,currentSystem] <- c(year, nbCities, meanLog, sdLog, SW.pvalue,  KS.pvalue)
    }
    
    resultDF
    blob <- ks.test(currentPops$sklogpop, "pnorm")
    
    ks.test(sort(as.numeric(currentPops$sklogpop)), "pnorm")
    shapiro.test(sort(as.numeric(currentPops$sklogpop)))
    
    
    
    
    rankedData <- BRICS %>%
        group_by(system, year) %>%
        mutate(rank = min_rank(-pop)) %>%
        mutate(year = as.factor(year))
    
    rankedData <- na.omit(rankedData)
    
    ggplot(data=rankedData, aes(x=rank, y=pop,group=year, colour=year), environment = environment()) + 
        geom_line(size=1) +
        scale_colour_gradient(low="skyblue3", high="firebrick4") +
        scale_alpha(range=c(0.1,1)) +
        scale_y_log10() +
        scale_x_log10() +
        facet_wrap(~system, scales = "fixed", ncol = 3) + 
        labs(title = "Rank-Size evolution",
             x = "Rank",
             y = "Population") +
        theme_bw()
    
    
    ##################
    
    grouped_summary <- BRICS %>%
        filter(!is.na(pop), system == "Brazil") %>%
        group_by(year) %>%
        summarise(nbCities = n(), totalPop = sum(pop, na.rm = TRUE))
    
    #grouped_summary
    df <- as.data.frame(t(grouped_summary))
    colnames(df) <- df[1,]
    
    growthratetable <- df[1:ncol(df)-1]
    # Creation des noms de périodes
    for (i in 1:ncol(growthratetable))
    {
        t0_name <- colnames(df[i])
        t1_name <- colnames(df[i+1])
        myname <- as.character(paste(t0_name, t1_name, sep="-"))
        colnames(growthratetable)[i] <- myname
    }
    growthratetable <- growthratetable[-1,]
    baseDF <- grouped_summary
    for (i in (2:nrow(baseDF))){
        changePop <- (baseDF[i,"totalPop"] - baseDF[i - 1,"totalPop"]) / baseDF[i - 1,"totalPop"]
        growthratetable[1,i-1] <- (baseDF[i,"nbCities"] - baseDF[i - 1,"nbCities"])
        growthratetable[2,i-1] <- (baseDF[i,"nbCities"] - baseDF[i - 1,"nbCities"]) / baseDF[i - 1,"nbCities"]
        growthratetable[3, i-1] <- (baseDF[i,"totalPop"] - baseDF[i - 1,"totalPop"])
        growthratetable[4, i-1] <- (baseDF[i,"totalPop"] - baseDF[i - 1,"totalPop"]) / baseDF[i - 1,"totalPop"]
    }
    growthratetable
    rownames(growthratetable) <- c("New Cities",  "% new cities", "New Pop", "% new pop")
    
    
    ####### BRICS TO CALCDF ######
    brazilLong <- BRICS %>%
        filter(system == "Brazil")
    
    brazilWide <- brazilLong %>%
        tidyr::spread(year, pop)
    
    test <- "Brazil"
    
    BRICS %>%
        filter_(sprintf("system == '%s'", test)) %>%
        glimpse()
    
    