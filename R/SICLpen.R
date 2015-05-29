SICLpen <- function(maplabels, annotmatrix){

  
  if(is.vector(maplabels) == FALSE) 
    stop(paste(sQuote("maplabels"), "must be a numeric vector"))
  
  if(is.matrix(annotmatrix) == FALSE) 
    stop(paste(sQuote("annotmatrix"), "must be a matrix"))
  
    penSICL <- 0
    for(i in 1:ncol(annotmatrix)){

        tab <- table(maplabels, annotmatrix[,i])
        #Leave at zero if one of clusters has 0 observations
        if(min(rowSums(tab)) > 0) {
            tab <- tab*log(tab/rowSums(tab))
            tab <- sum(ifelse(tab == "NaN", 0, tab))
        }
        penSICL <- penSICL + tab
    }
    return(penSICL)
}
