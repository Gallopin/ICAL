ICALpen <- function(maplabels, annotmatrix){
  
  
  if(is.vector(maplabels) == FALSE) 
    stop(paste(sQuote("maplabels"), "must be numeric vector"))
  
  if(is.matrix(annotmatrix) == FALSE) 
    stop(paste(sQuote("annotmatrix"), "must be a matrix"))
  

      penICAL <- 0
    for(i in  1:ncol(annotmatrix)){
        tab <- table(maplabels, annotmatrix[,i])
        #Leave at zero if one of clusters has 0 observations
        if(colSums(tab)[2] > 0) {
            divid <- colSums(tab)
            pen <- tab[,2]*log(tab[,2]/divid[2])
            pen <- sum(ifelse(pen == "NaN", 0, pen))
        } else {
            pen <- 0
        }
        penICAL <- penICAL + pen
    }
    return(penICAL)
}

