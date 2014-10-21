#' Coerces data.frames to xpssFrame objects
#'
#' Function to coerce a object to xpssFrame
#'
#' @usage as.xpssFrame(x)
#' @param x any (non-empty) R object. 
#' @author Bastian Wiessner
#' @details Coerce to xpssFrame.
#' @examples
#' # example data.frame
#' temp <- data.frame(x=1:5, y = 2:6, z=c("a","b","c","d","e"))
#' # coerce to xpssFrame
#' temp <- as.xpssFrame(temp)
#' @export

as.xpssFrame  <- function(x) {
  
  stopifnot(is.data.frame(x))
  
  if(class(x) == "xpssFrame")
  {
    stop("Data is already a xpssFrame object")
  }
    
  
  ## Attribute für Datensatz
  class(x) <- c("xpssFrame","data.frame")
  attr(x,"FILTER") <- FALSE
  attr(x,"TEMPORARY") <- FALSE  
  attr(x,"SPLIT_FILE") <- FALSE
  attr(x,"DO_IF") <- FALSE
  attr(x,"WEIGHTS") <- FALSE
  
  ### ---------------------- ###
  
  ## Attribute für Variablen
  
  for(i in 1:length(x)) {
  attr(x[[i]],"varname") <- paste(names(x[i]),sep="")  
  attr(x[[i]],"variable.label") <- paste(names(x[i]),sep="") 
  }
  return(x)
}

is.xpssFrame  <- function(x) {
  
     if(is.element("xpssFrame",class(x))) {
       return(T)
     }
}
