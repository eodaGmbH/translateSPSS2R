#' Creates numeric variables
#'
#' Creates new numeric variables, which get appended at the end of the dataset. 
#' 
#' xpssNumeric creates new numeric variables, which get appended at the end of the dataset. The new variables are as long as the selected dataset. By default the new variables are blank and get filled with NA, otherwise every case for the selected variable get filled with the speficied value. 
#'
#' @param x a (non-empty) data.frame, data.table object or input data of class \code{"xpssFrame"}. 
#' @param varname atomic character or character vector with the name of the variables which should be created.
#' @param fill values to fill the Variables. By default the value is NA for all new variables. It is possible to assign each new variable an own value to fill with.
#' @return Returns the input data extended by the new variables.
#' @author Andreas Wygrabek
#' @seealso \code{\link{xpssString}}
#' @examples \dontrun{
#' xpssNumeric(fromXPSS, varname = c("A"), fill = c(NA))
#' }
#' @export 


xpssNumeric <- function(x, varname = NULL, fill = NA){
    
  stopifnot(is.data.frame(x) | is.data.table(x) | class(x) == "xpssFrame")
  
  class(x) <- c("xpssFrame","data.frame","DM")
  ####################################################################
  ####################### Meta - Checks ##############################
  ####################################################################
  #x <- checkTemporary(x)
  
  
  x <- applyMetaCheck(x)
  
  ####################################################################
  ####################################################################
  ####################################################################
  
    if(is.matrix(x)){x <- as.data.frame(x)}
    if (length(fill) == 1){
    
            for(i in 1:length(varname)){
                
                eval(parse(text = paste("x$",varname[i]," <- ", fill, sep = "")))
                eval(parse(text =  paste("attributes(x$",varname[i],")","$varname", " <- ", varname[i],sep="")))
                eval(parse(text = paste("attributes(x$",varname[i],")","$variable.label", " <- ", varname[i],sep="")))
            } }else {
                    for(i in 1:length(varname)){
                        eval(parse(text = paste("x$",varname[i]," <- ", fill[i], sep = "")))
                        eval(parse(text =  paste("attributes(x$",varname[i],")","$varname", " <- ", varname[i],sep="")))
                        eval(parse(text = paste("attributes(x$",varname[i],")","$variable.label", " <- ", varname[i],sep="")))
                    }
    }
  
  
  
  ### DeMerge
  ## Filter is set & Function is a Datamanagement Function
  x <- applyAttributeDemerge(x)
  
    return(x)
}




