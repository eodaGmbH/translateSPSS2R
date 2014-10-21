#' Creates a subset of cases 
#'
#' xpssSelectIf permanently selects cases for analysis based on logical conditions. 
#'
#' The conditions to select cases are specified in a logical expression. These logical expressions can contain relational operators, logical operators, arithmetic operations. 
#' 
#'  \strong{NOTE:} For temporary case selection, specify a TEMPORARY command before SELECT IF.
#'
#' @param x a (non-empty) data.frame, data.table object or input data of class \code{"xpssFrame"}. 
#' @param cond logical expression for subsetting the data
#' @return Output is a subset of the actual dataset under the condition of the logical expression.
#' @author Andreas Wygrabek
#' @examples
#' 
#' #Select all Cases which match 1 in V3
#' 
#' data(fromXPSS)
#' temp <- xpssSelectIf(x=fromXPSS, cond = "V3 == 1")
#' 
#' temp <- xpssSelectIf(x=fromXPSS, cond="V4 == 1 & V7_1 < 200")
#' 
#' 
#' @export
xpssSelectIf <- function(x, cond = NULL){
    stopifnot(is.data.frame(x) | is.data.table(x) | "xpssFrame" %in% class(x))
    
    attr(x, "SELECT_IF") <- TRUE
      class(x) <- c("xpssFrame","data.frame","DM")  
    
  x <- applyMetaCheck(x)
  
    ################# TEMPORARY == TRUE
    if(attributes(x)$TEMPORARY == TRUE){
              
        # Attribute Backup
        attBack <- attributesBackup(x)
        # -
        
        x <- subset(x, subset = eval(parse(text = cond)))
        
        x <- applyAttributes(x, attBack)
        
    } else { ################# TEMPORARY == FALSE
        
        # Attribute Backup
        attBack <- attributesBackup(x)
        # -
        x <- subset(x, subset = eval(parse(text = cond)))
        
        x <- applyAttributes(x, attBack)
    }
    
    ### DeMerge
    ## Filter is set & Function is a Datamanagement Function
    x <- applyAttributeDemerge(x)
    
    
    return(x)
    
}




