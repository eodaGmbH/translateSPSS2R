#' Counts amount of specific Observations
#'
#' R Implementation of the SPSS \code{$COUNT} system variable
#'
#' Count displays the amount observations which match the count statement.
#'
#' @usage xpssCount(x, variables = NULL, count = NULL)
#'
#' @param x a (non-empty) data.frame, data.table object or input data of class \code{xpssFrame}. 
#' @param variables atomic character or character vector with the name of the variables.
#' @param count value to count
#' @return A vector of the same length as x.
#' @author Bastian Wiessner
#' @examples
#' data(fromXPSS)
#' 
#' xpssCount(x=fromXPSS, 
#'    variables = "V1", count="Nissan")
#' 
#' xpssCount(x=fromXPSS, 
#'    variables = "V5", count=2)
#' 
#' xpssCount(x=fromXPSS, 
#'    variables = c("V5","V5_kl2"), count=2)
#' @seealso Related Functions \code{\link{xpssAny}}
#' @export

xpssCount <- function(x,
                      variables = colnames(x),
                      count = NULL)
{
  stopifnot(is.data.frame(x) | is.data.table(x) | class(x) == "xpssFrame")

  # count index
x$count <- 0
 # temporären datensatz erstellen
temp <- x
 #schleife mit den variablen einleiten
for(l in 1:length(variables)){
  # zeilen zählen
  for(j in 1:nrow(x)){
    # variable in variabletemp schreiben
    variablesTemp <- variables[[l]]
    # leeren variablen vektor erstellen
    Varvec <- vector()
    # die variable zeilenweise durchgehen
    for(i in 1:length(variablesTemp)){
      # Vektor mit den Zeilenwerte erstellen 
      Varvec <- c(Varvec,x[j,c(variablesTemp[i])]) 
    }    
    # Anzahl an Treffern aufsummieren
    temp$count[[j]] <- sum(table(Varvec)[names(table(Varvec))%in%count])
    

  }
  # array mit den Treffer erstellen
  counterarray <- data.frame(x$count,temp$count)
  # Zeilensummen der Treffer aufsummieren
  x$count <- rowSums(counterarray)
}
return(x)
}

