#' Selects the first n rows of the dataset
#'
#' R implementation of the SPSS \code{N OF CASES} argument.
#' 
#' @usage xpssNOfCases(x, n = NULL)
#' @param x input data.
#' @param n atomic numeric with the value of n.
#' @return Output is the narrowed dataset.
#' @author Bastian Wiessner
#' @seealso Related Functions \code{\link{drop}} \code{\link{subset}} 
#' @examples 
#' # load data
#' data(fromXPSS)
#' xpssNOfCases(fromXPSS, n = 10)
#' @export
xpssNOfCases <- function(x, n = NULL){
  #do meta check
  functiontype <- "DM"
  x <- applyMetaCheck(x)
  
  # look if n is greather than the maximal row length
  if(n > nrow(x)){
    stop("n is greater than the maximal row length")
  }
  
  # create a 1:n subset
  x <- x[1:n,]
  
  ### DeMerge
  x <- applyAttributeDemerge(x)
  
  return(x)
}
