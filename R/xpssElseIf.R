#' Creates a subcondition within a DO IF - END IF subset
#'
#' R implementation of the SPSS \code{DO IF} argument. 
#'
#' After using \code{xpssDoIf} every following subset command will be initiated by \code{xpssElseIf}. \code{xpssElseIf} is working similar to \code{xpssDoIf}. Both functions select cases for analysis based on one or more logical conditions. The conditions to select cases are specified in a logical expression. These logical expressions can contain relational operators, logical operators and arithmetic operations. \code{xpssElseIf} creates a subset of the actual dataset, without deleting the excluded variables, respectively without deleting the excluded values.\cr \cr
#' 
#' The data is subsetted until \code{\link{xpssEndIf}} restores the excluded data. All changes made at the subsetted data will be taken over, the excluded data will remain untouched! As noted before, those cases are not actually deleted and will be available after \code{xpssEndIf} restores the excluded data. \cr \cr
#'
#'  In a different way to SPSS. Not only data management functions like \code{xpssRecode} can be used within \code{xpssElseIf}, it is possible to use statistical and descriptiv functions like \code{xpssFrequencies} too. \cr \cr
#'  \strong{NOTE:} For temporary case selection, specify \code{\link{xpssTemporary}} before \code{xpssDoIf}.

#' @usage xpssElseIf(x, cond = NULL)
#' @param x a (non-empty) data.frame or input data of class \code{"xpssFrame"}. 
#' @param cond logical expression indicating the condition for subsetting.
#' @return Output is a subset of the actual dataset under the condition of the logical expression.
#' @author Bastian Wiessner
#' @seealso Related Functions \code{\link{xpssDoIf}}, \code{\link{xpssEndIf}}, \code{\link{xpssFilter}}, \code{\link{xpssSelectIf}}, \code{\link{xpssTemporary}}
#' @examples
#' # load data
#' data(fromXPSS)
#' \dontrun{
#' # Select all cases which match 1 in V3
#' temp <- xpssDoIf(x=fromXPSS, cond = "V3 == 1")
#' # Recode all selected cases within V5
#' temp <- xpssRecode(x=temp,variables="V5",rec="lo:78 = 1; else = 2")
#' # Select all cases which match 2 in V3
#' temp <- xpssElseIf(x=fromXPSS, cond = "V3 == 1")
#' # Recode all selected cases within V5
#' temp <- xpssRecode(x=temp,variables="V5",rec="lo:78 = 11; else = 22")
# End DoIf Subsetting, restore dataset with applied changes
#' temp <- xpssEndIf(x=temp)}
#' @export

xpssElseIf <- function(x, cond = NULL){
  
  if(attributes(x)$DO_IF == FALSE){
    stop("An introducing xpssDoIf command is necessary to use xpssElseIf")
  }
  
  # End If Part
  #------------
    
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
  
  #-------------------------
  
  #check if necessary
  #backup attributes again
  attr_backup <- attributesBackup(x)
  
  functiontype <- "ME"
  x <- applyMetaCheck(x)
  
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
