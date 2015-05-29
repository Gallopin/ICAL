\name{ICAL-package}
\alias{ICAL-package}
\alias{ICAL}
\docType{package}
\title{
ICAL
}
\description{
This package computes ICAL and SICL penalty to perform model selection in model-based clustering of annotated data
}
\details{
\tabular{ll}{
Package: \tab ICAL\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2015-05-15\cr
License: \tab WGPL (>= 3)\cr
}
%~~ An overview of how to use the package, including the most important functions ~~
}
\author{


Maintainer: melina.gallopin@gmail.com <melina.gallopin@gmail.com>
%~~ The author and/or maintainer of the package ~~
}
\references{
~~ Literature or other references for background information ~~
}
~~ Optionally other standard keywords, one per line, from file KEYWORDS in the R ~~
~~ documentation directory ~~
\keyword{ package }
\seealso{
~~ Optional links to other man pages, e.g. ~~
~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
}
\examples{


## load Rmixmod package to perform clustering
library(Rmixmod)

## load expression data and annotations data from data files
data(annotations)
data(expression)

############################
## Clustering using Rmixmod
############################
## clustering settings
strategy <- mixmodStrategy()
myModel <- mixmodGaussianModel(listModels="Gaussian_pk_Lk_Bk")
## clustering
res <- mixmodCluster(data.frame(expression), 1:10, criterion=c("BIC", "ICL"), model= myModel)


############################
## model selection
############################
listNbCluster <- unlist(lapply(res@results, function(mixmodResultsObjet) mixmodResultsObjet@nbCluster))
listBIC <- unlist(lapply(res@results, function(mixmodResultsObjet) mixmodResultsObjet@criterionValue[1]))
listICL <- unlist(lapply(res@results, function(mixmodResultsObjet) mixmodResultsObjet@criterionValue[2]))
listPartition <- lapply(res@results, function(mixmodResultsObjet) mixmodResultsObjet@partition)
listSICLpen <- unlist(lapply(listPartition,function(onePartition) SICLpen(onePartition, annotations)))
listSICL <- listICL -2*listSICLpen
listICALpen <- unlist(lapply(listPartition,function(onePartition) ICALpen(onePartition, annotations)))
listICAL <- listICL -2*listICALpen

############################
## return results 
############################
selectedModelSICL <- res@results[[which.min(listSICL)]]
selectedModelICAL <- res@results[[which.min(listICAL)]]

############################
## plot data and results
############################
# plot expression data with annotations
par(mfrow = c(1,2), mar=c(1,1,1,1))
plot(expression, ylim=c(-5,12),xlim=c(-3,12), col=1, xlab="variable 1", ylab="variable 2", pch=1, cex=1.5)
points(expression[annotations[,1]==1,], col=2, pch=2, lwd=3)
points(expression[annotations[,2]==1,], col=3, pch=3, lwd=3)
points(expression[annotations[,3]==1,], col=4, pch=4, lwd=3)
plot(0,bty="n",col="white",xaxt="n",yaxt="n",main="",ylab="",xlab="")
legend("bottomleft", c("data", "annotation 1","annotation 2","annotation 3"), col=c(1,2,3,4), pch=c(1,2,3,4))

## plot criteria
par(mfrow = c(1,1), mar=c(4,4,4,4))
plot(listNbCluster[order(listNbCluster)], -listBIC[order(listNbCluster)], main="", ylab="criterion value", type="l", xlab="number of clusters",
     col="black", lty=3, lwd=2, ylim=c(-3500,-1600))
points(listNbCluster[which.min(listBIC)], -listBIC[which.min(listBIC)], col="black",pch=1, lwd=4)
lines(listNbCluster[order(listNbCluster)], -listICL[order(listNbCluster)], main="ICL", type="l", xlab="K",
      ylab="valeur du crit??re", col="red", lty=2, lwd=2)
points(jitter(listNbCluster[which.min(listICL)]), jitter(-listICL[which.min(listICL)]), col="red",pch=1, lwd=5)
lines(listNbCluster[order(listNbCluster)], -listSICL[order(listNbCluster)], main="SICL", type="l", xlab="K",
      ylab="valeur du crit??re", col=3, lty=4, lwd=2)
points(listNbCluster[which.min(listSICL)], -listSICL[which.min(listSICL)], col="green",pch=1, lwd=4)
lines(listNbCluster[order(listNbCluster)], -listICAL[order(listNbCluster)], main="ICAL", type="l", xlab="K",
      ylab="valeur du crit??re", col=4, lty=1, lwd=2)
points(listNbCluster[which.min(listICAL)], -listICAL[which.min(listICAL)], col="blue",pch=1, lwd=4)
legend("bottomright", c("BIC", "ICL","SICL","ICAL"), col=c(1,2,3,4),lty=c(3,2,4,1),lwd = c(2,2,2,2))


## plot clusters for BIC, ICL, ICAL and SICL model selection
par(mfrow = c(2,2), mar=c(1,1,1,1))
nbClu <-length(unique(listPartition[[which.min(listBIC)]]))
plot(expression[listPartition[[which.min(listBIC)]]==1, ], ylim=c(-5,12),xlim=c(-3,12), col=1, main="BIC", xlab="variable 1", ylab="variable 2", pch=1, cex=1.5)
for(i in 1:length(unique(listPartition[[which.min(listBIC)]]))){
  points(expression[listPartition[[which.min(listBIC)]]==i, ], ylim=c(-5,12),xlim=c(-3,12), col=i, pch=1, cex=1.5)  
}
nbClu <-length(unique(listPartition[[which.min(listICL)]]))
plot(expression[listPartition[[which.min(listICL)]]==1, ], ylim=c(-5,12),xlim=c(-3,12), col=1, main="ICL", xlab="variable 1", ylab="variable 2", pch=1, cex=1.5)
for(i in 1:length(unique(listPartition[[which.min(listICL)]]))){
  points(expression[listPartition[[which.min(listICL)]]==i, ], ylim=c(-5,12),xlim=c(-3,12), col=i, pch=1, cex=1.5)  
}
nbClu <-length(unique(listPartition[[which.min(listSICL)]]))
plot(expression[listPartition[[which.min(listSICL)]]==1, ], ylim=c(-5,12),xlim=c(-3,12), col=1, main="SICL", xlab="variable 1", ylab="variable 2", pch=1, cex=1.5)
for(i in 1:length(unique(listPartition[[which.min(listSICL)]]))){
  points(expression[listPartition[[which.min(listSICL)]]==i, ], ylim=c(-5,12),xlim=c(-3,12), col=i, pch=1, cex=1.5)  
}
nbClu <-length(unique(listPartition[[which.min(listICAL)]]))
plot(expression[listPartition[[which.min(listICAL)]]==1, ], ylim=c(-5,12),xlim=c(-3,12), col=1, main="ICAL", xlab="variable 1", ylab="variable 2", pch=1, cex=1.5)
for(i in 1:length(unique(listPartition[[which.min(listICAL)]]))){
  points(expression[listPartition[[which.min(listICAL)]]==i, ], ylim=c(-5,12),xlim=c(-3,12), col=i, pch=1, cex=1.5)  
}
}
