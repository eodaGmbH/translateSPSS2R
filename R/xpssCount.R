#' Counts frequencies of specific observations
#'
#' R implementation of the SPSS \code{$COUNT} system variable.
#'
#' Count displays the frequencies of observations matching the count statement.
#'
#' @usage xpssCount(x, variables = NULL, count = NULL)
#'
#' @param x a (non-empty) data.frame, data.table object or input data of class \code{xpssFrame}. 
#' @param variables atomic character or character vector with the name of the variables.
#' @param count atomic character or atomic numeric pattern.
#' @return A vector of the same length as x.
#' @author Bastian Wiessner
#' @examples
#' 
#' # load data
#' data(fromXPSS)
#' 
#' # count Nissan in variable V1
#' xpssCount(x=fromXPSS, 
#'    variables = "V1", count=list(exact="Nissan"))
#' 
#' # count 2 in variable V5
#' xpssCount(x=fromXPSS, 
#'    variables = "V5", count=list(exact=2))
#'
#' # count in V5 to V7_2, lowest values until 100
#' xpssCount(fromXPSS, 
#'    variables = span(fromXPSS, from = "V5",
#'                              to = "V7_2"), 
#'                    count = list(from = "lo",
#'                                   to = 100))
#' @seealso Related Functions \code{\link{xpssAny}} \code{\link{\%in\%}} \code{\link{is.element}}
#' @export

xpssCount <- function(x,
                      variables = NULL,
                      count = NULL)
{
  
  # meta check
  functiontype <- "SB"
  x <- applyMetaCheck(x)
  
  x <- xpssValue(x)
  
  #abfangen der special characters
  mintemp  <- maxtemp<- vector()
  if(!is.null(count)) {
    if("lo" %in% count) {
      for(i in 1:length(variables)){
        evalVAR <- eval(parse(text = paste("x$",variables[[i]],sep="")))
        mintemp <- c(min(evalVAR,na.rm=T),mintemp)
      }
      pos <- which(count %in% "lo" |count %in% "lowest")  
      count[[pos]] <- min(mintemp)
      
    }
    if("hi" %in% count) {
      for(i in 1:length(variables)){
        evalVAR <- eval(parse(text = paste("x$",variables[[i]],sep="")))
        maxtemp <- c(max(evalVAR,na.rm=T),mintemp)
      }
      pos <- which(count %in% "hi" |count %in% "highest")  
      count[[pos]] <- max(maxtemp,na.rm=T)
    }
  }
  # count index
  x$count <- 0
  # temporären datensatz erstellen
  temp <- x
  Varvec <- vector()
  #schleife mit den variablen einleiten
  if(length(count$exact)>0){
    for(l in 1:length(variables)){
      # count rows
      for(j in 1:nrow(x)){
        # write variables in variabletemp
        variablesTemp <- variables[[l]]
        # generate an empty variable vector
        Varvec <- vector()
        # check variables row for row
        for(i in 1:length(variablesTemp)){
          # generate a vector with the values of the rows 
          Varvec <- c(Varvec,x[j,c(variablesTemp[i])]) 
        }    
        # Sum up count of hits
        temp$count[[j]] <- sum(table(Varvec)[names(table(Varvec))%in%count])
      }
      # create an array with all search hits
      counterarray <- data.frame(x$count,temp$count)
      # add up row sums of the search hits
      x$count <- rowSums(counterarray)
    }
  }
  else {    
    count_min <- as.numeric(count$from)
    count_max <- as.numeric(count$to)
    for(l in 1:length(variables)){      
      evalVAR <- eval(parse(text = paste("x$",variables[[l]],sep="")))
      for(j in 1:length(evalVAR)){
        if((is.na(evalVAR[j]) == FALSE) && (round(count_min,digits=5) <= round(evalVAR[j],digits=5)) && (round(evalVAR[j],digits=5) <= round(count_max,digits=5))){
          x$count[[j]] <- x$count[[j]]+1
        }
      }
    }
  }
  return(x)
}