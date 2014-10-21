#' Creates string variables
#'
#' Creates new string variables, which get appended at the end of the dataset. 
#' 
#' xpssString creates new string variables, which get appended at the end of the dataset. The new variables are as long as the selected dataset. By default the new variables are blank and get filled with NA, otherwise every case for the selected variable gets filled with the filled value. 
#'
#' @param x a (non-empty) data.frame, data.table object or input data of class "xpssFrame". 
#' @param varname atomic character or character vector with the name of the variables which should be created.
#' @param fill values to fill the Variables. By default the value is NA for all new variables. It is possible to assign each new variable an own value to fill with.
#' @return Returns the input data extended by the new variables
#' @author Andreas Wygrabek
#' @seealso \code{\link{xpssNumeric}}
#' @examples
#' 
#' data(fromXPSS)
#' xpssString(fromXPSS, varname = c("D","E"), fill = c("placeholder",NA))
#' @export
xpssString <- function(x, varname = NULL, fill = NA){
  
  stopifnot(is.data.frame(x) | is.data.table(x) | "xpssFrame" %in% class(x))
  
  class(x) <- c("xpssFrame","data.frame","DM")
  ####################################################################
  ####################### Meta - Checks ##############################
  ####################################################################
  #x <- checkTemporary(x)
  
  
  x <- applyMetaCheck(x)
  
  ####################################################################
  ####################################################################
  ####################################################################
  
  if (length(fill) == 1){
    
    for(i in 1:length(varname)){
      eval(parse(text = paste("x$",varname[i]," <- ", "'",fill,"'", sep = "")))
    } }else {
      for(i in 1:length(varname)){
        eval(parse(text = paste("x$",varname[i]," <- ","'", fill[i],"'", sep = "")))
      }
      
    }
  
  
  ### DeMerge
  ## Filter is set & Function is a Datamanagement Function
  x <- applyAttributeDemerge(x)
  
  return(x)
}