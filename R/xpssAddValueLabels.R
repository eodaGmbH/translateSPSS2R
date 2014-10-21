#' Modifies value labels
#'
#' xpssAddValueLabels appends value labels for specific variables. The values of the label get stored in attributes of the variable.
#'
#' @param x a (non-empty) data.frame or input data of class \code{"xpssFrame"}. 
#' @param variables atomic character or character vector with the name of the variables.
#' @param values atomic numeric or numeric vector containing the value of the variable.
#' @param labels character string containing a variable's label.
#' @param datevariables atomic date or date vector with the name of the date variables.
#' @param datevalues atomic date or date vector containing the value of the date, the value has to be like the old date format.
#' @param datelabels character string containing a date's label.
#' @details The values labels are stored in the variable itself. 
#' \cr In contrast to \code{\link{xpssValueLabels}} , \code{\link{xpssAddValueLabels}} does not erase existing value labels. \cr If the value label for a specific variable already exists, this value label gets overwritten. \cr If the value label for a specific variable does not exist, the value label gets created, without deleting the existing value labels for that variable.
#' @author Bastian Wiessner
#' @seealso \code{\link{read.spss}} \code{\link{xpssValueLabels}} \code{\link{xpssVariableLabels}} 
#' @examples
#' 
#' data(fromXPSS)
#' 
#' temp <- xpssValueLabels(fromXPSS, 
#'                            variables = "V1", 
#'                            value = 1 ,
#'                            label = "Label1")
#'
#' temp <- xpssAddValueLabels(temp, 
#'                            variables = "V1", 
#'                            value = 2 ,
#'                            label = "Label2")
#'                            
#' attributes(temp$V1)$value.labels
#'                            
#'                            
#' @export
xpssAddValueLabels <- function(x, variables = NULL, values = NULL, labels = NULL,
                            datevariables = NULL, datevalues = NULL, datelabels = NULL){
  
  options(warn=-1)
  

  class(x) <- c("xpssFrame","data.frame","DM")
  ####################################################################
  ####################### Meta - Checks ##############################
  ####################################################################
  
  x <- applyMetaCheck(x)
  
  ####################################################################
  ####################################################################
  ####################################################################
  
  
  if((!is.data.frame(x) & !is.data.table(x)) | !("xpssFrame" %in% class(x))){
    stop("Object has to be from class data.frame, data.table or xpssFrame")
  }
  
  if(length(values) != length(labels)){
    stop("Vectors values and labels don´t have the same length")
  }
  
  if("NA" %in% labels){
    stop("Using NA as label is not possible")
  }
  if(!is.null(values))
  {
    if(class(values) != "numeric"){
      stop("Only numeric values are allowed for the argument values")
    }
  } 
  
  
  if(length(variables) > 0) {
    for(i in 1:length(variables)){
      
      if(class(variables[i]) != "numeric" | class(variables[i]) != "factor"){
        "Input-Variables from variables have to be numeric or factor"
      }
      
      names(values) <- labels

      if(is.null(attr(x[,variables[i]], "value.labels"))) {
        attr(x[,variables[i]], "value.labels") <- values  
      } else {
        
        # value ist existent
        for(j in 1:length(values)) {
          if(isTRUE(values[[j]] %in% attr(x[,variables[i]], "value.labels"))) {
            pos <- which(attr(x[,variables[i]], "value.labels") %in% values)
            attr(x[,variables[i]], "value.labels")[pos] <- values
            names(attr(x[,variables[i]], "value.labels"))[pos] <- names(values)
          } else {
            # value ist nicht existent
            pos <- which(!(values %in% attr(x[,variables[i]], "value.labels")))
            attr(x[,variables[i]], "value.labels")[length(attr(x[,variables[i]], "value.labels"))+1] <- values[pos]
            names(attr(x[,variables[i]], "value.labels"))[length(attr(x[,variables[i]], "value.labels"))] <- names(values[pos])
          }
        }
      }
    }
  }
  
  if(length(datevariables) >0 ){
    for(i in 1:length(datevariables)){
      
      if(class(datevariables[i]) != "date" | class(datevariables[i]) != "POSIXlt" | class(datevariables[i]) != "POSIXt" | class(datevariables[i]) != "POSIXct"){
        "Input-Variables from datevariables have to be class date or POSIX"
      }
      
      names(datevalues) <- datelabels
      
      if(is.null(attr(x[,variables[i]], "value.labels"))) {
        attr(x[,variables[i]], "value.labels") <- datevalues  
      } else {
        
        # value ist existent
        for(j in 1:length(datevalues)) {
          if(isTRUE(datevalues[[j]] %in% attr(x[,variables[i]], "value.labels"))) {
            pos <- which(attr(x[,variables[i]], "value.labels") %in% datevalues)
            attr(x[,variables[i]], "value.labels")[pos] <- datevalues
            names(attr(x[,variables[i]], "value.labels"))[pos] <- names(datevalues)
          } else {
            # value ist nicht existent
            pos <- which(!(datevalues %in% attr(x[,variables[i]], "value.labels")))
            attr(x[,variables[i]], "value.labels")[length(attr(x[,variables[i]], "value.labels"))+1] <- datevalues[pos]
            names(attr(x[,variables[i]], "value.labels"))[length(attr(x[,variables[i]], "value.labels"))] <- names(datevalues[pos])
          }
        }
      }
    }
  }
  
  ### DeMerge
  ## Filter is set & Function is a Datamanagement Function
  x <- applyAttributeDemerge(x)
  
  options(warn=0)
  return(x)
}


