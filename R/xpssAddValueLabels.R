#' Modifies value labels
#'
#' R implementation of the SPSS \code{ADD VALUE LABELS} function. \code{xpssAddValueLabels} appends value labels for specific variables. Those values labels get stored in the attributes of the selected variable.
#'
#' @param x a (non-empty) data.frame or input data of class \code{"xpssFrame"}. 
#' @param variables atomic character or character vector with the name of the variables. 
#' @param values atomic numeric or numeric vector, respectively as an atomic character or character vector containing the value of the variable. 
#' @param labels atomic numeric or numeric vector, respectively as an atomic character or character vector containing the label of the variable.
#' @param datevariables atomic date or date vector with the name of the date variables. 
#' @param datevalues atomic date or date vector containing the value of the date, the value has to be like the old date format. 
#' @param datelabels atomic numeric or numeric vector, respectively as an atomic character or character vector containing a variable label. 
#' @details The values labels are stored in the variable itself. 
#' \cr In contrast to \code{\link{xpssValueLabels}} , \code{xpssAddValueLabels} do not erase existing value labels. \cr If the value label for a specific variable already exists, this value label gets overwritten. \cr If the value label for a specific variable does not exist, the value label gets created without deleting the existing value labels for that variable.
#' @return An xpssFrame object with modified value labels.
#' @author Bastian Wiessner
#' @seealso \code{\link{read.spss}} \code{\link{xpssValueLabels}} \code{\link{xpssVariableLabels}} 
#' @examples
#' 
#' # load data
#' data(fromXPSS)
#' 
#' # create a value label for variable V1
#' fromXPSS <- xpssValueLabels(fromXPSS, 
#'                            variables = "V1", 
#'                            value = 1 ,
#'                            label = "Label1")
#'                            
#' # add another value label for variable V1
#' fromXPSS <- xpssAddValueLabels(fromXPSS, 
#'                            variables = "V1", 
#'                            value = 2 ,
#'                            label = "Label2")
#'                            
#' # show value labels for variable V1
#' attributes(fromXPSS$V1)$value.labels
#'                            
#'                            
#' @export
xpssAddValueLabels <- function(x, variables = NULL, values = NULL, labels = NULL,
                            datevariables = NULL, datevalues = NULL, datelabels = NULL){
  
  options(warn=-1)
  
  ####################################################################
  ####################### Meta - Checks ##############################
  ####################################################################
  
  functiontype <- "DM"
  x <- applyMetaCheck(x)
  
  ####################################################################
  ####################################################################
  ####################################################################

  if(!(is.element(variables,names(x)))) {
    stop("The selected variable has to be in the dataset")
  }
  
  if(length(values) != length(labels)){
    stop("Vector values and vector labels dont have the same length, this problem could occur if label or value is NULL")
  }
  
  if("NA" %in% labels){
    stop("Using NA as label is not possible")
  }
  if(!is.null(values))
  {
    if(class(values) != "numeric" && class(values) != "character"){
      stop("Only numeric or character values are allowed for the argument values")
    }
  } 
  
  
  if(length(variables) > 0) {
    for(i in 1:length(variables)){
      
      if(class(variables[i]) != "numeric" | class(variables[i]) != "factor"){
        "Input Variables from variables have to be numeric or factor"
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
        "Input Variables from datevariables have to be class date or POSIX"
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
  ## Filter is set and Function is a Datamanagement Function
  x <- applyAttributeDemerge(x)
  
  options(warn=0)
  return(x)
}


