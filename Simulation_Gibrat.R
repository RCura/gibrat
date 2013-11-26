#######################################################
# Outil de comparaison des trajectoires réelles de    #
# systèmes de villes et de leur modélisation          #
# selon les lois de Gibrat.                           #
#######################################################
suppressWarnings(source("/home/robin/Dropbox/ParisGeo/2.0 - Vacations GeoDC/1 - GeoDiverCity/2 - Outil-Gibrat/fonctions.R"))
#####################
# PARAMÈTRES
#####################

filepath <- "/home/robin/Dropbox/ParisGeo/2.0 - Vacations GeoDC/1 - GeoDiverCity/2 - Outil-Gibrat/villesFr1831.csv" # Remplacer par "Explorateur" si le chemin n'est pas connu
#filepath <- "Explorateur"
simNb <- 100 # Nombre de simulations à lancer


#####################
# INITIALISATION
#####################

outil.obsdata <- mon_import_csv(file=filepath)
#####################
# SIMULATION
#####################
#library(multicore)
# On lance les réplications
system.time(outil.simulations <- run_simulation(df=outil.obsdata, reps=simNb))
# On ajoute les noms de villes, dates et simulations
dimnames(outil.simulations) <- list(rownames(outil.obsdata), colnames(outil.obsdata), paste("Sim", 1:simNb, sep=""))
remove(simNb)
########################
# ANALYSE DES RÉSULTATS
########################
# On crée un tableau contenant la distribution finale de chaque réplication.
outil.simResults <- outil.simulations[,ncol(outil.obsdata),]
# On calcule la moyenne de population des villes à chaque pas de temps
outil.simulationMean <- apply(X=outil.simulations[,,], 1:2, mean)
# On calcule l'ecart-type des logs de population des villes à chaque pas de temps
outil.simulationSD <- apply(X=log(outil.simulations[,,]), 1:2, sd)
# On passe à la création d'un tableau des rangs de la moyenne des simulations.
MeanRanks <- create_rank_tables(obsdata=outil.obsdata, simMean=outil.simulationMean)
outil.simRank <- as.data.frame(MeanRanks[1]) ; outil.obsRank <- as.data.frame(MeanRanks[2])
remove(MeanRanks)
colnames(outil.simRank) <- colnames(outil.obsRank) <- colnames(outil.obsdata)
# On crée un tableau avec les R² des corrélations observées/simulées des distributions finales de villes pour chaque réplication.
outil.correlationMatrix <- as.matrix(unlist(lapply(1:dim(outil.simulations)[[3]],function (x) return(cor(log(outil.obsdata[ncol(outil.obsdata)]),y=log(outil.simulations[,ncol(outil.obsdata),x]))))))
dimnames(outil.correlationMatrix) <- dimnames(outil.simulations)[3]
# On crée un tableau avec les R² des corrélations observées/simulées des trajectoires de villes pour chaque réplication.
outil.rowCorrelationMatrix <- matrix(data=unlist(lapply(X=1:nrow(outil.obsdata),FUN= function (x) return(cor(x=outil.simulations[x,,1:dim(outil.simulations)[[3]]],y=as.double(outil.obsdata[x,]),use="pairwise.complete.obs",method="spearman")))), nrow=nrow(outil.obsdata),ncol=dim(outil.simulations)[[3]],byrow=TRUE,dimnames=list(rownames(outil.obsdata),colnames(outil.simResults)))

#####################
# GRAPHIQUES
#####################

# On cherche le max et le min dans simList
lastTime <- ncol(outil.obsdata)
maxpop <- max(max(outil.obsdata[lastTime]),max(outil.simulations[,lastTime,]))
minpop <- min(min(outil.obsdata[lastTime]),min(outil.simulations[,lastTime,]))


par(mfrow=c(1,1), oma=c(0,0,2,0))

plot(x=sort(rank(-outil.obsdata[,lastTime]), decreasing=T), y=sort(outil.obsdata[,lastTime], decreasing=F), log="xy", type="l", ylim=c(minpop,maxpop), xlab="Rang", ylab="Population", col="darkblue", lwd=2)
# Création du graphe pour toutes les sims :
for (i in 1:dim(outil.simulations)[3])
{
    lines(x=sort(rank(-outil.simulations[,lastTime,i]), decreasing=T), y=sort(outil.simulations[,lastTime,i], decreasing=F), col="darkgrey", lwd=1)
}
lines(x=sort(rank(-outil.simulationMean[,lastTime]), decreasing=T), y=sort(outil.simulationMean[,lastTime], decreasing=F), col="firebrick", lwd=2)
lines(x=sort(rank(-outil.obsdata[,lastTime]), decreasing=T), y=sort(outil.obsdata[,lastTime], decreasing=F), col="darkblue", lwd=2)
title("Courbes Rang-Taille observées et simulées en fin de période", outer=TRUE)
legend(x="topright", "Observé", cex=0.7, seg.len=4, col="darkblue" , lty=1, lwd=2 )
legend(x="bottomleft", "Simulées", cex=0.7, seg.len=4, col="darkgrey" , lty=1 )
legend(x="bottomright", "Moyenne des simulations", cex=0.7, seg.len=4, col="firebrick" , lty=1, lwd=2 )

#####################
# NUMÉRIQUES
#####################

cat("Corrélation ville obs/ville sim pour chaque réplication, résumé par réplication.")
#summary(outil.rowCorrelationMatrix)

cat("Sd Observé : ", sd(log(outil.obsdata[,ncol(outil.obsdata)])), " / Sd Simulé : ", sd(log(outil.simulationMean[,ncol(outil.simulationMean)])))
cat("Corrélation entre les logarithmes des populations finales observées et simulées : ", cor(x=log(outil.obsdata[,ncol(outil.obsdata)]), y=log(outil.simulationMean[,ncol(outil.simulationMean)]), use="pairwise.complete.obs", method="spearman"))
cat("Corrélation entre les rangs finaux observés et simulés : ", cor(x=outil.obsRank[,ncol(outil.obsRank)], y=outil.simRank[,ncol(outil.simRank)], use="pairwise.complete.obs", method="spearman"))
cat("Corrélations entre les données observées et les simulations, log des tailles : \n",summary(outil.correlationMatrix))