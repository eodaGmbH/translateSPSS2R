#' Creates a subset of cases 
#'
#' R Implementation of the SPSS \code{SELECT IF} argument. xpssSelectIf permanently selects cases for analysis based on logical conditions. 
#'
#' The condition to select cases are specified in a logical expression. These logical expressions can contain relational operators, logical operators and arithmetic operations. 
#' 
#'  \strong{NOTE:} For temporary case selection, specify \code{\link{xpssTemporary}} before \code{SELECT IF}.
#'
#' @param x a (non-empty) data.frame or input data of class \code{"xpssFrame"}. 
#' @param cond logical expression for subsetting the data.
#' @return Returns a subset of the actual dataset under the condition of the logical expression.
#' @author Andreas Wygrabek
#' @seealso Related Functions \code{\link{xpssDoIf}}, \code{\link{xpssFilter}}, \code{\link{xpssTemporary}}
#' @examples
#' 
#' # load data
#' data(fromXPSS)
#' 
#' # Select all ases which match 1 in V3
#' temp <- xpssSelectIf(x=fromXPSS, cond = "V3 == 1")
#' 
#' # Select all cases which match 1 in V3 and are greater than 200 in V7_1
#' temp <- xpssSelectIf(x=fromXPSS, cond="V4 == 1 & V7_1 < 200")
#' 
#' 
#' @export
xpssSelectIf <- function(x, cond = NULL){
    
    ####################################################################
    ####################### Meta - Checks ##############################
    ####################################################################
    
    functiontype <- "ME"
    x <- applyMetaCheck(x)
    
    ####################################################################
    ####################################################################
    ####################################################################
              
        # Attribute Backup
        attBack <- attributesBackup(x)
        # -
        
        x <- subset(x, subset = eval(parse(text = cond)))
        
        x <- applyAttributes(x, attBack)
        attr(x, "SELECT_IF") <- cond        
  
    ### DeMerge
    ## Filter is set & Function is a Datamanagement Function
    x <- applyAttributeDemerge(x)
    
    
    return(x)
    
}




