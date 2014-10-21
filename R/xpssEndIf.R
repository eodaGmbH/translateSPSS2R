#' Creates a subset of cases 
#'
#' xpssEndIf determines the end of the analysis based on logical conditions via xpssDoIf.
#'
#' \code{\link{xpssEndIf}} restores the original data, after \code{\link{xpssDoIf}} subsetted the data. All changes which were made until \code{\link{xpssEndIf}} will be taken over, the excluded data will be untouched!\cr \cr
#' 
#'  \strong{NOTE:} For temporary case selection, specify \code{\link{xpssTemporary}} before \code{\link{xpssDoIf}}.
#'
#' @usage xpssEndIf(x)
#' @param x a (non-empty) data.frame or input data of class \code{"xpssFrame"}. 
#' @return Output is a  the original dataset.
#' @author Andreas Wygrabek
#' @examples
#' data(fromXPSS)
#' # Select all cases which match 1 in V3
#' temp <- xpssDoIf(x=fromXPSS, cond = "V3 == 1")
#' # Recode all selected cases
#' temp <- xpssRecode(x=temp,varin="V5",rec="lo:78 = 1; else = 2")
#' # End DoIf Subsetting, restore dataset with applied changes
#' temp <- xpssEndIf(x=temp)
#' 
#' @export
xpssEndIf <- function(x){
    
    stopifnot(is.data.frame(x) | is.data.table(x) | "xpssFrame" %in% class(x))
    
    # Set DO_IF attribute to TRUE
    attributes(x)$DO_IF <- FALSE
    
    # Attribute Backup
    attBack <- attributesBackup(x)
    # -
    
    x <- rbind(x,attributes(x)$DO_IF_INVERSE)
    
    # Sort the data by rownames
    x <- x[order(as.numeric(rownames(x))),] 

    # ApplyAttributes
    x <- applyAttributes(x, attBack)
    
    # Delete DO_IF_Reverve 
    attributes(x)$DO_IF_INVERSE <- NULL
    
    return(x)
}