#' Creates a subset without deleting cases
#'
#' R Implementation of the SPSS \code{filter} argument
#'
#' xpssFilter creates a subset of the actual dataset, without deleting the excluded variables, respectively without deleting the excluded values. After activating \code{\link{xpssFilter}}, only the subset will be used, the excluded data gets ignored for the following actions until  \code{\link{xpssFilterOff}} terminates the filtering. As noted before those cases are \strong{not actually} deleted and will be available after the filter is turned off. 
#' \cr\cr \strong{Important:} 
#' \cr All changes are used on the complete dataset, except the function is a \emph{data exploring} or \emph{data analyzing} function \cr \cr
#'  \tabular{rlll}{
#'  \tab Type of Function \tab Example Function \tab Dataset Usage \cr
#' \tab Data Management  \tab \code{\link{xpssSelectIf}} \tab Uses complete dataset\cr
#' \tab Data Modifing \tab \code{\link{xpssRecode}} \tab Uses complete dataset\cr
#' \tab Data Exploring \tab \code{\link{xpssDescriptives}} \tab Uses working dataset only\cr
#' \tab Data Analyzing \tab \code{\link{xpssRegression}} \tab Uses working dataset only\cr
#'}
#' @usage xpssFilter(x,variable = NULL, filtervalue = 1)
#' @param x a (non-empty) data.frame, data.table object or input data of class \code{"xpssFrame"}. 
#' @param variable one or or more variables given as character. 
#' @param filtervalue atomic character or atomic numeric which contains the filtervalue.
#' @return Output is a subset of the actual dataset under the predetermined condition of the filtervalue.
#' @author Bastian Wiessner
#' @seealso Related Functions \code{\link{xpssDoIf}} \code{\link{xpssFilterOff}} \code{\link{xpssSample}} \code{\link{xpssSelectIf}} \code{\link{xpssTemporary}} 
#' @examples 
#' data(fromXPSS)
#' # Filter Dataset by Variable V3 == 1
#' fromXPSS <- xpssFilter(x=fromXPSS, variable = "V3", filtervalue=1)
#' # Default Descriptive statistics for V6
#' xpssDescriptives(x=fromXPSS, variables = "V6")
#' # Turn Filter off
#' xpssFilterOff(x=fromXPSS)
#' @export
xpssFilter <- function(x, variable = NULL, filtervalue = 1){

  stopifnot(is.data.frame(x) | is.data.table(x) | "xpssFrame" %in% class(x))
    
  #Exception if the old Filter gets replaced by a new Filter
  if(is.null(attributes(x)$FILTERED_DATA) == FALSE) {
    x <- xpssFilterOff(x)   
  }  
  # set positionvectfor for matches
  pos <-  which(x[,variable] == filtervalue)
  
  # set Filter as the condition
  attr(x, "FILTER") <- paste0(variable, " == ", filtervalue)
  # write the data which dont match in filtered data
  attr(x, "FILTERED_DATA") <- x[setdiff(1:nrow(x),pos),]
  
  # ATTRIBUTES BACKUP
  attBack <- attributesBackup(x)
  # -
  
  # overwrite the old data which the matched values from the condition
  x <- x[pos,]
  
  # filter notification
  print("Filter is activated: Keep the rownames to switch the filter off")
  
  # APPLY ATTRIBUTES
  x <- applyAttributes(x, attBack)
  # -
  
  return(x)
}




