#' Stores Attributes 
#'
#' Attribut backup function
#'
#' The conditions to select cases are specified in a logical expression. These logical expressions can contain relational operators, logical operators, arithmetic operations. 
#' 
#'  \strong{NOTE:} For temporary case selection, specify a TEMPORARY command before SELECT IF.
#'
#' @param x a (non-empty) data.frame, data.table object or input data of class \code{"xpssFrame"}. 
#' @param cond logical expression for subsetting the data
#' @return Output is a subset of the actual dataset under the condiftion of the logical expression.
#' @author Bastian Wiessner
#' @examples
#' data(fromXPSS)
#' data <- xpssSelectIf(fromXPSS, cond = "V3 == 1")
#' @keywords internal
attributesBackup <- function(x) {
  
  if(!is.data.frame(x) && !("xpssFrame" %in% class(x))){
      stop("x has to be a xpssFrame or data.frame")
  }
     
  attrs <- list("global","local")
  # global attributes
  
  attrs$global <- attributes(x)
  
  # local attributes
  
  attrs$local <- sapply(x, function(x){
      attributes(x)
  })
  
#   ### Deprecated
#   vars <- paste("x$",attrs$global$names,sep="")
#   for(i in 1:length(vars))
#   {
#     attrs$local[[i]] <- attributes(eval(parse(text=paste("x$",attrs$global$names[i],sep=""))))
#   }
  ### End Depricated Block
    
  class(attrs) <- c("xpssAttributes", "list")
    
  return(attrs)
}




