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
axis(side = 1, at=ticks, labels=FALSE, las=1)
text(cex=.8, x=ticks, y=-0.02, labels, xpd=TRUE, srt=45, pos=1)
lines(dnorm(x = 0:max(log(baseData)), mean = mean(log(baseData)), sd = sd(log(baseData))), type = "l", col = "blue", lwd = 2)

baseData2 <- data.frame(x = baseData, y = baseData + 10E3, z = log(baseData), w = log(baseData + 10E3))
fit1 <- fitdistr(baseData2$x, densfun = "lognormal")
fit2 <- fitdistr(baseData2$z, densfun = "normal")
ggplot(data=baseData2,  aes(x=x)) + geom_histogram(aes(y = ..density..), binwidth=10E3) +
    stat_function(fun = dnorm, size=1, color='blue', args = list(fit1$estimate[1], fit1$estimate[2]))

