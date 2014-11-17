#' Temporary modification of the data
#'
#' xpssTemporary modifies only the data for the following data-management procedure. 
#' 
#' xpssTemporary signals the beginning of temporary transformation. Only the following data-management procedure takes affect on the data: xpssRecode, xpssCount, xpssDoIf, xpssSelectIf, xpssSample, xpssFilter, xpssNumeric, xpssString, xpssVariableLabels, xpssValueLabels, xpssMissingValues. All the changes that are made are temporary. After the next modification the data is restored. 
#' For example:  all created variables, e.g. numeric or string variables created while the TEMPORARY is in effect are temporary variables! 
#' Any changes or modifications made to existing variables while the TEMPORARY command is in effect are also temporary! 
#' Any variables which are created or modified after this procedure are again permanent. 
#' 
#' The xpssTemporary function allows analyses for subgroups without affecting the data and then repeat the analysis for the file as a whole. 
#'
#' @param x a (non-empty) data.frame, data.table object or input data of class "xpssFrame". 
#' @author Andreas Wygrabek
#' @examples
#' data(fromXPSS)
#' obj <- xpssTemporary(fromXPSS)
#' @export
xpssTemporary <- function(x){
  
  functiontype <- "ME"
  x <- applyMetaCheck(x)
  
  attr(x, "ORIGIN") <- x

    attributes(x)$TEMPORARY <- TRUE
    
    return(x)
}
