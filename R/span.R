#' Indicates range of varlist
#'
#' Creates a list of variables within a specific range
#'
#' @usage span(x, from = NULL, to = NULL, addDF = FALSE)
#' @param x a (non-empty) data.frame, data.table object or input data of class "xpssFrame". 
#' @param from variable that opens the span
#' @param to variable that closes the span
#' @param addDF Should the name of the input data be used?
#' @return Returns a varlist with the name of the variables which a within the range of the span indicator. 
#' @author Andreas Wygrabek
#' @examples 
#' foo <- matrix(data = c(1:15), 
#' nrow = 3,
#' ncol = 5,
#' dimnames = list(c(1,2,3),c("A","B","C","D","E")))
#' span(foo, from = "C", to = "E",addDF = FALSE)
#' span(foo, from = "C", to = "E",addDF = TRUE)
#' @export
span <- function(x = data, from = NULL, to = NULL, addDF = FALSE){

    if(!(is.character(from) & is.character(to))){
        stop("from/to has to be character")
    } else {
    
    
    startindex <- which(colnames(x) == from)
    endindex <- which(colnames(x) == to)
    
    if(addDF != TRUE && addDF !=  FALSE) {
      stop("only TRUE or FALSE are allowed as Statement")
    }
    
    if(addDF == FALSE){
        varlist <- colnames(x)[ startindex : endindex ]
        return(varlist)
    } else {
    varlist <- eval(paste(substitute(x),"$", colnames(x)[ startindex : endindex ], sep=""))
    return(varlist)
        }
    }
}





