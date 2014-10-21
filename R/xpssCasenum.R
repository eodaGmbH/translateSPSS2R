#' Sequential numbering of cases
#'
#' R Implementation of the SPSS \code{$CASENUM} system variable.
#'
#' The xpssCasenum counts the number of cases within a variable, respectively the number of observations in the dataset.
#' 
#'  This statement can be used as ID-Variable.
#'
#' @usage xpssCasenum(x)
#' @param x a (non-empty) data.frame or input data of class \code{"xpssFrame"}. 
#' @return Returns an sequential atomic numeric or numeric vector.
#' @author Bastian Wiessner
#' @seealso Related Functions \code{\link{xpssDate}} , \code{\link{xpssDate11}}
#' @examples
#' data(fromXPSS)
#' fromXPSS$id <- xpssCasenum(fromXPSS)
#' @export

xpssCasenum <- function(x)
{
  stopifnot(is.data.frame(x) | is.data.table(x) | class(x) == "xpssFrame")
  
  casenum <- 1:length(x[[1]])
  return(casenum)
}