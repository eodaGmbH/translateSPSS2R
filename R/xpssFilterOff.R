#' Displays the content of variables.
#'
#' R Implementation of the SPSS \code{LIST} Function
#'
#' LIST displays the content of selected variables. It's possible to display a sequenz with the \code{cases} argument.
#'
#' @usage xpssFilterOff(x)
#' @param x a (non-empty) data.frame, data.table object or input data of class \code{xpssFrame}. 
#' @return If cases is not specified the return is the length of the data.
#' @author Bastian Wiessner
#' @examples
#' lala <- 1
#' @export
xpssFilterOff <- function(x){
    
  if(attributes(x)$FILTER == FALSE){
      stop("Filter not activated")
  }
  
  x <- rbind(x,attributes(x)$FILTERED_DATA)
  attBack <- attributesBackup(x)

  
  # Sort the data by rownames
  x <- x[order(as.numeric(rownames(x))),]  
  x <- applyAttributes(x, attBack)
  
  attributes(x)$FILTER  <-  FALSE
  attributes(x)$FILTERED_DATA <- NULL
  
  return(x)
}











