#' Creates a subset of cases 
#'
#' xpssDoIf selects cases for analysis based on one or more logical conditions. 
#'
#' The conditions to select cases are specified in a logical expression. These logical expressions can contain relational operators, logical operators and arithmetic operations. \cr \cr
#' 
#' The data is subsetted until \code{\link{xpssEndIf}} restores the original data. All changes made at the subsetted data will be taken over, the excluded data will be untouched!\cr \cr
#'
#'  In a different way to SPSS. Not only data management functions like \code{\link{xpssRecode}} can be used within \code{xpssDoIf}, it is possible to use statistical and descriptiv functions like xpssFrequencies too. \cr \cr
#'  \strong{NOTE:} For temporary case selection, specify \code{\link{xpssTemporary}} before \code{\link{xpssDoIf}}.

#' @usage xpssDoIf(x, cond = NULL)
#' @param x a (non-empty) data.frame or input data of class \code{"xpssFrame"}. 
#' @param cond logical expression for subsetting the data
#' @return Output is a subset of the actual dataset under the condition of the logical expression.
#' @author Andreas Wygrabek
#' @seealso Related Functions \code{\link{xpssEndIf}}, \code{\link{xpssFilter}}, \code{\link{xpssSelectIf}}, \code{\link{xpssTemporary}}
#' @examples
#' data(fromXPSS)
#' # Select all cases which match 1 in V3
#' temp <- xpssDoIf(x=fromXPSS, cond = "V3 == 1")
#' # Recode all selected cases within V5
#' temp <- xpssRecode(x=temp,varin="V5",rec="lo:78 = 1; else = 2")
#' # End DoIf Subsetting, restore dataset with applied changes
#' temp <- xpssEndIf(x=temp)
#' @export
xpssDoIf <- function(x, cond = NULL){
    
  #backup attributes
  attr_backup <- attributesBackup(x)
  
    stopifnot(is.data.frame(x) | is.data.table(x) | "xpssFrame" %in% class(x))
    
    # Put origin dataset to attributes
    attr(x, "DO_IF_INVERSE") <- x
    
    # Set DO_IF attribute to TRUE
    attributes(x)$DO_IF <- cond
    
    # Attribute Backup
    attBack <- attributesBackup(x)
    # -
    
    # Replace the object data with a subset 
    x <- subset(x, subset = eval(parse(text = cond)))
    
    # Reduce the attribute data
    
    x <- applyAttributes(x, attBack)
    
    logVec <-  !is.element(rownames(attributes(x)$DO_IF_INVERSE), rownames(x))
    attributes(x)$DO_IF_INVERSE <- attributes(x)$DO_IF_INVERSE[logVec,]
    
    attributes(x)$DO_IF_INVERSE <- applyAttributes(attributes(x)$DO_IF_INVERSE, attBack)
  
    
    return(x)
}


