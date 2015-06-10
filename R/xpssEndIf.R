#' Ends a DO IF - END IF subset
#'
#' R implementation of the SPSS \code{END IF} argument.
#' 
#'   \code{xpssEndIf} determines the end of the analysis based on logical conditions via \code{\link{xpssDoIf}}.  \code{xpssEndIf} merge the excluded data with the actual dataset, after \code{xpssDoIf} subsetted the data. All changes which were made until \code{xpssEndIf} will be taken over, the excluded data will remain untouched!\cr \cr
#' 
#'  \strong{NOTE:} For temporary case selection, specify \code{xpssTemporary} before \code{xpssDoIf}.
#'
#' @usage xpssEndIf(x)
#' @param x a (non-empty) data.frame or input data of class \code{"xpssFrame"}. 
#' @return Output is the original dataset.
#' @author Andreas Wygrabek
#' @examples
#' # load data
#' data(fromXPSS)
#' 
#' # Select all cases matching 1 in V3
#' temp <- xpssDoIf(x=fromXPSS, cond = "V3 == 1")
#' 
#' # Recode all selected cases
#' temp <- xpssRecode(x=temp,variables="V5",rec="lo:78 = 1; else = 2")
#' 
#' # End DoIf Subsetting, restore dataset with applied changes
#' temp <- xpssEndIf(x=temp)
#' 
#' @export
xpssEndIf <- function(x){
    
  functiontype <- "ME"
  x <- applyMetaCheck(x)
    
    # Set DO_IF attribute to TRUE
    attributes(x)$DO_IF <- FALSE
    
    # Attribute Backup
    attBack <- attributesBackup(x)
    # -
    
  # if there was a compute call b4
  if(length(x) != length(attributes(x)$DO_IF_INVERSE)){
    cols <- which(!(names(x) %in% names(attributes(x)$DO_IF_INVERSE)))
    attributes(x)$DO_IF_INVERSE[names(x[cols])] <- NA
  }
  
    x <- rbind(x,attributes(x)$DO_IF_INVERSE)
    
    # Sort the data by rownames
    x <- x[order(as.numeric(rownames(x))),] 

    # ApplyAttributes
    x <- applyAttributes(x, attBack)
    
    # Delete DO_IF_Reverve 
    attributes(x)$DO_IF_INVERSE <- NULL
    
    return(x)
}