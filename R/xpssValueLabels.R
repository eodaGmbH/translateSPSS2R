#' Modifies value labels
#'
#' xpssValueLabels creates value labels for specific variables. The values of the label get stored in attributes of the variable.
#'
#' @param x a (non-empty) data.frame, data.table object or input data of class "xpssFrame". 
#' @param variables atomic character or character vector with the name of the variables.
#' @param values atomic numeric or numeric vector containing the value of the variable.
#' @param labels character string containing a variable's label.
#' @param datevariables atomic date or date vector with the name of the date variables.
#' @param datevalues atomic date or date vector containing the value of the date, the value has to be like the old date format.
#' @param datelabels character string containing a date's label.
#' @details The SPSS variables are stored at the variable itself. 
#' \cr In contrast to \code{\link{xpssAddValueLabels}} , \code{\link{xpssValueLabels}} does erase existing value labels. \cr If the value label for a specific variable already exists, all value labels for that variable get overwritten. \cr If the value label for a specific variable does not exist, the value label gets created and all existing value labels for that variable get deleted. 
#' \cr\cr A variable can have the following attributes: 
#' value.labels, defined.MIS, MIS, varname, variable.label
#' @author Andreas Wygrabek
#' @seealso \code{\link{read.spss}}
#' @examples
#' 
#' data(fromXPSS)
#' 
#' temp <- xpssValueLabels(fromXPSS, 
#'                            variables = "V1", 
#'                            value = 1 ,
#'                            label = "Label1")
#'                            
#'                            
#' @export
xpssValueLabels <- function(x, variables = NULL, values = NULL, labels = NULL,
                            datevariables = NULL, datevalues = NULL, datelabels = NULL){
    
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
   
   class(x) <- c("xpssFrame","data.frame","DM")
   ####################################################################
   ####################### Meta - Checks ##############################
   ####################################################################
   #x <- checkTemporary(x)
   
   
   x <- applyMetaCheck(x)
   
   ####################################################################
   ####################################################################
   ####################################################################
   
   
   if(length(variables) > 0) {
       for(i in 1:length(variables)){
         
         if(class(variables[i]) != "numeric" | class(variables[i]) != "factor"){
           "Input-Variables from variables have to be numeric or factor"
         }
         
         names(values) <- labels
         attr(x[,variables[i]], "value.labels") <- values
       }
     }
     
     if(length(datevariables) >0 ){
       for(i in 1:length(datevariables)){
         
         if(class(datevariables[i]) != "date" | class(datevariables[i]) != "POSIXlt" | class(datevariables[i]) != "POSIXt" | class(datevariables[i]) != "POSIXct"){
           "Input-Variables from datevariables have to be class date or POSIX"
         }
         
         names(datevalues) <- datelabels
         attr(x[,datevariables[i]], "value.labels") <- datevalues
       }
     }
   
   
   ### DeMerge
   ## Filter is set & Function is a Datamanagement Function
   x <- applyAttributeDemerge(x)
   
   return(x)
   }
    



    
    
    