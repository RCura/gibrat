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
