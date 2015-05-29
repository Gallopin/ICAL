\name{ICALpen}
\alias{ICALpen}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
ICAL penalty for model selection of model-based clustering of annotated data
}
\description{
%% This function implements the ICAL criterion for model-based clustering of annotated data.

This function computes the ICAL penalty used in the ICAL criterion for model-based clustering of annotated data. 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{

ICALpen <- function(maplabels, annotmatrix)
  
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{maplabels}{
  Vector of length \emph{n} containing the cluster assignments of the \emph{n} observations
}
  \item{annotmatrix}{
%%     ~~Describe \code{u} here~~
(\emph{n} x \emph{m}) matrix containing the \emph{m} annotations vectors for the \emph{n} observations
}
}
\details{


%%  ~~ If necessary, more details than the description above ~~
}
\value{

 \item{penICAL}{the ICAL penalty value}
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
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
listICALpen <- unlist(lapply(listPartition,function(onePartition) ICALpen(onePartition, annotations)))
listICAL <- listICL -2*listICALpen

############################
## return results 
############################
selectedModelICAL <- res@results[[which.min(listICAL)]]



% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~ICALpenalty }
\keyword{ ~clustering }% __ONLY ONE__ keyword per line
