#' Renaming Variables
#'
#' xpssRenameVariables renames Variables within an exisiting data.frame or xpssFrame object.
#'
#' Modifies names of one or more variables within a selected dataset.
#'
#' @param x a (non-empty) data.frame, data.table object or input data of class \code{"xpssFrame"}. 
#' @param oldVarNames Character vector naming the variables in the data set to rename.
#' @param newVarNames Character vector with the new variable names. Has to be the same length as oldVarNames.
#' @return The functions returns the data x with new variable names.
#' @author Andreas Wygrabek
#' @examples 
#' foo <- matrix(data = c(1:9), 
#' nrow = 3,
#' ncol = 3,
#' dimnames = list(c(1,2,3),c("B","C","D")))
#' foo <- as.data.frame(foo)
#' foo <- as.xpssFrame(foo)
#' xpssRenameVariables(foo, c("B", "C", "D"), c("A", "B", "C"))
#' @export
xpssRenameVariables <- function(x, oldVarNames = NULL, newVarNames = NULL){

  stopifnot(is.data.frame(x) | is.data.table(x) | class(x) == "xpssFrame")

  class(x) <- c("xpssFrame","data.frame","DM")
  ####################################################################
  ####################### Meta - Checks ##############################
  ####################################################################
  #x <- checkTemporary(x)
  
  
  x <- applyMetaCheck(x)
  
  ####################################################################
  ####################################################################
  ####################################################################
  
  oldVarNames <- c(oldVarNames)
  newVarNames <- c(newVarNames)
  colnames(x)[colnames(x) %in% oldVarNames] <- newVarNames
  
  pos <- which(colnames(x) %in% newVarNames)
  
  for(i in 1:length(newVarNames)) {
    attributes(x[[pos[i]]])$varname <- newVarNames[i]  
  }
  
  ### DeMerge
  ## Filter is set & Function is a Datamanagement Function
  x <- applyAttributeDemerge(x)
  

  return(x)
}




