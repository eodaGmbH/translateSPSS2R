#' Displays whether a variable contains missing values
#'
#' R Implementation of the SPSS \code{Missing} Function.
#'
#' Performs a missing value operation. \code{xpssMissing} displays system- and user-defined missing values of variables.
#' The different missing types will be inspected without checking if they are system-; or user-defined missings.
#' 
#' \code{variable} input is limited to only one variable.
#' 
#' @usage xpssMissing (x, variable = NULL)
#' @param x a (non-empty) data.frame, data.table object or input data of class "xpssFrame". 
#' @param variable atomic character with the name of the variable.
#' @return atomic logical with the length of the data. Returns \code{TRUE},
#' if the variable contains a system- or user-defined missing value.
#' @author Bastian Wiessner
#' @seealso Related Functions \code{\link{xpssNmiss}} , \code{\link{xpssNvalid}} , \code{\link{xpssSysmis}} ,\code{\link{xpssValue}}
#' @examples
#' # load data
#' data(fromXPSS)
#' 
#' # display missings in variable V7_2
#' xpssMissing(fromXPSS, variable="V7_2")
#' @export

xpssMissing <- function(x, variable = NULL){

  
  ####################################################################
  ####################### Meta - Checks ##############################
  ####################################################################
  
  functiontype <- "DM"
  x <- applyMetaCheck(x)
  
  ####################################################################
  ####################################################################
  ####################################################################
  
  
  LOGMAT <- matrix(0, ncol = length(variable), nrow = nrow(x))
  
  NUM <- sapply(x[,variable], function(x){
    is.numeric(x)})
  CHA <- sapply(x[,variable], function(x){
    is.character(x)})
  FAC <- sapply(x[,variable], function(x){
    is.factor(x)})
  
  if(length(variable) == 1)
  {
    LOGMAT <- is.na(x[,variable])
    OUT <- LOGMAT  
  }
  else{
    stop("Function is specified for only one Variable")
  }
  
  return(OUT)
}

