#' Modifies variable labels
#'
#' Changing the label of a variable. In the structure of xpss-data the variable label is an attribute of each variable.
#'
#' @param x a (non-empty) data.frame, data.table object or input data of class "xpssFrame". 
#' @param variables Variable(s) to change the label. Given as a character string
#' @param labels Labels for the specified variables in  variables. Given as a character string. The labels are associated in order of appearence
#' to  variables.   
#' @return Input Data with modified attribute variable label
#' @author Andreas Wygrabek
#' @seealso \code{\link{attributes}} \code{link{attr}}
#' @examples
#' data(fromXPSS)
#' daten <- xpssVariableLabels(fromXPSS, c("V4", "V7_1"), c("Label1", "Label2"))
#' @export
xpssVariableLabels <- function(x,  variables = NULL, labels = NULL){
    
  if((!is.data.frame(x) & !is.data.table(x)) | !("xpssFrame" %in% class(x))){
    stop("Object has to be from class data.frame, data.table or xpssFrame")
  }
  class(x) <- c("xpssFrame","data.frame","DM")
  ####################################################################
  ####################### Meta - Checks ##############################
  ####################################################################
  #x <- checkTemporary(x)
  
  
  x <- applyMetaCheck(x)
  
  ####################################################################
  ####################################################################
  ####################################################################
  
    myList <- as.list( variables)
    
    if(length( variables) != length(labels)){
        stop("Length of  variables and labels has to be the same")
    }
    
    for(i in 1:length( variables)){
    attr(x[, variables[[i]]], "variable.label") <- labels[[i]]
    }
  
  
  ### DeMerge
  ## Filter is set & Function is a Datamanagement Function
  x <- applyAttributeDemerge(x)
  
    return(x)
}
    




