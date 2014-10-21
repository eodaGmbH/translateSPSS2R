#' Displays the amount of valid values in variables.
#'
#' R Implementation of the SPSS \code{Nvalid} Function
#'
#' Performs a missing value operation. As opposite of \code{xpssNmiss}, \code{xpssNvalid} visualizies only the valid values of integer vectors or character vectors.
#'
#' \code{variables} variables have to be same type.
#'
#' @usage xpssNvalid (x, variables = NULL)
#'
#' @param x a (non-empty) data.frame, data.table object or input data of class \code{"xpssFrame"}. 
#' @param variables atomic character or character vector with the name of the variables.
#' @return atomic numeric with the length of the data. Returns the amount of valid values of the variables.
#' @author Bastian Wiessner
#' @seealso Related Functions \code{\link{xpssMissing}} , \code{\link{xpssNmiss}} , \code{\link{xpssSysmis}} ,\code{\link{xpssValue}}
#' @examples
#' data(fromXPSS)
#' xpssNvalid(fromXPSS, variables=c("V6","V7_2"))
#' @export

xpssNvalid <- function(x, variables = NULL){
    
    stopifnot(is.data.frame(x) | is.data.table(x) | class(x) == "xpssFrame")
    
    LOGMAT <- matrix(0, ncol = length(variables), nrow = nrow(x))
    
    NUM <- sapply(x[,variables], function(x){
        is.numeric(x)})
    CHA <- sapply(x[,variables], function(x){
        is.character(x)})
    FAC <- sapply(x[,variables], function(x){
        is.factor(x)})
    
    for(i in 1:length(variables)){
      
      attribut_indicator <- attr(x[,variables[i]],"MIS")[,1] 
      na_indicator <- which(is.na(x[,variables[i]]))
      pos <- c(attribut_indicator, na_indicator)
      
      x[,variables[[i]]][pos] <- NA
        
    if(length(variables) == 1)
    {
      LOGMAT[,i] <- !is.na(x[,variables[i]])
      OUT <- rowSums(LOGMAT)
      
    } else if(sum(NUM) == length(variables) | sum(CHA) == length(variables) | sum(FAC) == length(variables)){
      LOGMAT[,i] <- !is.na(x[,variables[i]]) 
      OUT <- rowSums(LOGMAT)
    } else {
        stop("Variables are not from the same type")
    }

    }
    return(OUT)
}