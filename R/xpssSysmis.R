#' Displays if a variable contains system-missing values
#'
#' R implementation of the SPSS \code{Sysmis} Function
#'
#' Performs a missing value operation. Displays only the amount of system missing values of the variable instead of looking after all missing values only system-missings get inspected.
#'
#' \code{variable} is limited to one variable.
#'
#' @usage xpssSysmis (x, variable = NULL)
#'
#' @param x a (non-empty) data.frame, data.table object or input data of class "xpssFrame". 
#' @param variable atomic character with the name of the variable.
#' @return atomic numeric with the length of the data. Returns the amount of system missing values of the variable.
#' @author Bastian Wiessner
#' @seealso Related Functions \code{\link{xpssMissing}} , \code{\link{xpssNmiss}} , \code{\link{xpssNvalid}} ,\code{\link{xpssValue}}
#' @examples
#' 
#' # load data
#' data(fromXPSS)
#' 
#' # locate sysmis in variable V7_2
#' xpssSysmis(fromXPSS, variable="V7_2")
#' @export

xpssSysmis <- function(x, variable = NULL){
  
  functiontype  <- "SB"
  x <- applyMetaCheck(x)

    if(length(variable) == 1)
    {
        pos <- attr(x[,variable],"MIS")[,1]
        x[,variable][pos] <- attr(x[,variable],"MIS")[,2]
        logvec <- is.na(x[,variable])
    }
    else{
      stop("Function is specified for only one Variable")
    }
 
  return(logvec)
}

