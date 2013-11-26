test <- mydata.data
colnames(test) <- sub(pattern="X", replacement="", x=colnames(test))
test[,34] <- apply(X=test[,2:32], FUN=sum, MARGIN=1)
colnames(test)[33] <- "Classe"
colnames(test)[34] <- "Somme"
test_pc <- test[2:34]
test_pc[1:31] <- NA
test_pc <- as.matrix(test_pc)
a <- Sys.time()
for (row in 1:471)
{
  for (col in 2:32)
  {
    test_pc[row, col - 1] <- test[row, col] / test[row, 34]
  }
}

moyenneclasses <- matrix(nrow=6, ncol=32)
colnames(moyenneclasses)[2:32] <- colnames(test_pc)[1:31]
colnames(moyenneclasses)[1] <- "Classe"
moyenneclasses[,1] <- 1:6

for (i in 1:6)
{
  for (j in 2:32)
  {
    moyenneclasses[i,j] <- mean(test_pc[test_pc[,32]==i,j-1])
  }
}
print(Sys.time() - a)
par(mfrow=c(1,1), oma=c(0,0,2,0))
plot(x=colnames(moyenneclasses)[2:32],y=moyenneclasses[1,2:32], ylim=c(min(moyenneclasses[,2:32]),max(moyenneclasses[,2:32])),xlab="Temps", ylab="% pop moyen",type="o", pch=4, col=1 )
classnames <- "Classe 1"
for (i in 2:6)
{ 
  classname <- paste("Classe",i,sep=" ")
  classnames <- c(classnames, classname)
  lines(x=colnames(moyenneclasses)[2:32],y=moyenneclasses[i,2:32], main=classname, type="o" , pch=4, col=i)
}
legend(x="topleft", classnames, cex=0.7, seg.len=4, col=1:nbclasses, pch=4, lty=1, )
title("Courbes moyennes (% pop totale/ an) des classes de la CAH", outer=TRUE)