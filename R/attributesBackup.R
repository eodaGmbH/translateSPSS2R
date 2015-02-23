#' Stores Attributes 
#'
#' Attribut backup function
#'
#' The conditions to select cases are specified in a logical expression. These logical expressions can contain relational operators, logical operators, arithmetic operations. 
#' 
#'  \strong{NOTE:} For temporary case selection, specify a TEMPORARY command before SELECT IF.
#'
#' @param x a (non-empty) data.frame, data.table object or input data of class \code{"xpssFrame"}. 
#' @return Output is a subset of the actual dataset under the condiftion of the logical expression.
#' @seealso \code{\link{attributes}} \code{\link{attr}}
#' @author Bastian Wiessner
#' #' @examples
#' #load data
#' data(fromXPSS)
#' #  display dataset attribtues
#' attributes(fromXPSS)
#' #  display attribtues of variable V7_2
#' attributes(fromXPSS$V7_2)
#' # save attributes
#' x <- attributesBackup(fromXPSS)
#' # reorder data
#' fromXPSS <- fromXPSS[order(fromXPSS$V2),] 
#' #  display dataset attribtues; still there
#' attributes(fromXPSS)
#' #  display attribtues of variable V7_2; missing
#' attributes(fromXPSS$V7_2)
#' # add missing attributes
#' fromXPSS <- applyAttributes(fromXPSS, x)
#' #  display dataset attribtues
#' attributes(fromXPSS)
#' #  display attribtues of variable V7_2
#' attributes(fromXPSS$V7_2)
#' @export
attributesBackup <- function(x) {
  
#   if(!is.data.frame(x) && !("xpssFrame" %in% class(x))){
#       stop("x has to be a xpssFrame or data.frame")
#   }
     
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




