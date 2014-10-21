#' Displays the content of variables.
#'
#' R Implementation of the SPSS \code{LIST} Function
#'
#' LIST displays the content of selected variables. It's possible to display a sequenz with the \code{cases} argument.
#'
#' @usage xpssList(x, variables = "", cases = FALSE,from = NULL, to = NULL, by = NULL)
#'
#' @param x a (non-empty) data.frame, data.table object or input data of class \code{xpssFrame}. 
#' @param variables atomic character or character vector with the name of the variables.
#' @param cases Not documented yet
#' @param from Not documented yet
#' @param to Not documented yet
#' @param by Not documented yet
#' @return If cases is not specified the return is the length of the data.
#' @author Bastian Wiessner
#' @examples
#' data(fromXPSS)
#' 
#' xpssList(x=fromXPSS)
#' 
#' xpssList(x=fromXPSS, 
#'    variables = "V1")
#' 
#' xpssList(x=fromXPSS, 
#'    variables = c("V1","V2"))
#' 
#' xpssList(x=fromXPSS, 
#'    variables = span(fromXPSS,
#'                  from="V1",
#'                  to="V4"),
#'    cases =list(from=2,
#'                to=18,
#'                by=2))
#' 
#' @export

xpssList <- function(x,
                     variables = colnames(x),
                     cases = list(from = 1,
                                  to = nrow(x),
                                  by = 1)) 
{
  stopifnot(is.data.frame(x) | is.data.table(x) | class(x) == "xpssFrame")

  if(is.null(cases$from))
  {
    cases$from  <-1
  }     
  if(is.null(cases$to)) {
    cases$to <- nrow(x)
  }
  if(is.null(cases$by))  {
    cases$by <- 1
  }
  
  if(!is.numeric(cases$from) || !is.numeric(cases$to) || !is.numeric(cases$by))  {
    stop("the arguments for from, to and by have to numeric")
  }
  if(cases$to > nrow(x)) {
    stop("to argument is bigger then the dataset")
  }
  
    pos <- seq(cases$from,cases$to,cases$by)
    erg <- data.frame(1:length(pos))
    if(length(variables) == 1){      
      erg <- as.data.frame(x[,variables][pos],stringsAsFactors=F)
      names(erg) <- attr(x[,variables], "varname")
    }else{
      for(i in 1:length(variables)){
        erg[[i]] <-   as.data.frame(x[,variables[i]][pos])
        names(erg[[i]]) <- attr(x[,variables[i]], "varname")
      }
    }
  return(erg) 
}



 