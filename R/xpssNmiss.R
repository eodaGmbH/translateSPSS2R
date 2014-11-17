#' Displays the amount of missing values in variables.
#'
#' R implementation of the SPSS \code{Nmiss} function
#'
#' Performs a missing value operation. \code{xpssNmiss} displays the amount of system- and user-defined missing values of the variables. \cr User-defined and system-defind missings values get handled as one type of missing value.
#'
#' @usage xpssNmiss(x, variables = NULL)
#' @param x a (non-empty) data.frame, data.table object or input data of class \code{"xpssFrame"}. 
#' @param variables atomic character or character vector with the names of the variables.
#' @return atomic numeric with the length of the data. Returns the amount of
#' system- or user-defined missing values of the variables.
#' @author Bastian Wiessner
#' @seealso Related Functions \code{\link{xpssMissing}} , \code{\link{xpssNvalid}} , \code{\link{xpssSysmis}} ,\code{\link{xpssValue}}
#' @examples
#' # load data
#' data(fromXPSS)
#' 
#' # display the amount of user- and system-defined missing values in variable V6 and V7_2
#' xpssNmiss(fromXPSS, variables=c("V6","V7_2"))
#' @export


xpssNmiss <- function(x, variables = NULL){
  
  ####################################################################
  ####################### Meta - Checks ##############################
  ####################################################################
  
  functiontype <- "DM"
  x <- applyMetaCheck(x)
  
  ####################################################################
  ####################################################################
  ####################################################################
  
  
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
      LOGMAT[,i] <- (is.na(x[,variables[i]]))
      OUT <- rowSums(LOGMAT)
      
    } else if(sum(NUM) == length(variables) | sum(CHA) == length(variables) | sum(FAC) == length(variables)){
      LOGMAT[,i] <- is.na(x[,variables[i]]) 
      OUT <- rowSums(LOGMAT)
    } else {
      stop("Variables are not from the same type")
    }
    
  }
  return(OUT)
}
