#' Displays the value of the variable cases.
#'
#' R implementation of the SPSS \code{Value} function.
#'
#' Performs a missing value operation. Adds the user-defined missing values of the variables in the existing dataset.
#'
#' \code{variables} variables have to be same type.
#'
#' @usage xpssValue(x, variables = NULL)
#' @param x a (non-empty) data.frame, data.table object or input data of class "xpssFrame". 
#' @param variables atomic character or character vector with the names of the variables.
#' @return A "xpssFrame" object or (data frame) with implemented user-defined missing values. 
#' @author Bastian Wiessner
#' @seealso Related Functions \code{\link{xpssMissing}} , \code{\link{xpssNmiss}} , \code{\link{xpssNvalid}} , \code{\link{xpssSysmis}}
#' @examples
#' # load data
#' data(fromXPSS)
#' 
#' # add user defined values for variable V7_2  
#' xpssValue(fromXPSS, variables="V7_2")
#'
#' # add user defined values for variable V6 and V7_2   
#' temp <- xpssValue(fromXPSS, variables=c("V6","V7_2"))
#' 
#' @export

xpssValue <- function(x, variables = NULL){
  
  ####################################################################
  ####################### Meta - Checks ##############################
  ####################################################################
  
  functiontype <- "DM"
  x <- applyMetaCheck(x)
  
  ####################################################################
  ####################################################################
  ####################################################################
  
  
  attBack <- attributesBackup(x)
  
  if(is.null(variables)) {
    for(i in 1:length(x[1,])) {
      attribut_indicator <- attributes(x[[i]])$MIS[,1] 
      x[[i]][attribut_indicator] <- attributes(x[[i]])$MIS[,2] 
    }
  } else {
    
    for(i in 1:length(variables)){
      attribut_indicator <- attr(x[,variables[i]],"MIS")[,1] 
      x[,variables[i]][attribut_indicator] <- attr(x[,variables[i]],"MIS")[,2] 
    }
  }
  
  x <- applyAttributes(x=x,attributesToApply=attBack)
  
  return(x)
}