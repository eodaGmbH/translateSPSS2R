#' Apply Meta Checks
#'
#' Checks the attributes for eventual merging the data
#'
#' Helper Function for Merging Attributedatasets
#' 
#' @param x a (non-empty) data.frame, data.table object or input data of class \code{"xpssFrame"}. 
#' @return Output is the input data merged on the basis of the condition in the attributes of the actual dataset.
#' @author Bastian Wiessner
#' @keywords internal
#' @export

applyMetaCheck <- function(x) {
  

  # check if the class is supported
  if(is.data.frame(x) == F && is.data.table(x) == F && ("xpssFrame" %in% class(x) ==F)) {
    stop("Input data has to be a data.frame or xpssFrame",call.=F)
  }
  
  # get the function type
  functiontype <- get("functiontype", parent.frame())
    
  # coerce if a meta function gets applied on a data.frame
  if(functiontype == "ME") {
    # if data isnt a xpssframe object
    if(!is.element("xpssFrame",class(x))) {
      # pull warning
      message("actual data got coerced to xpssFrame, due missing of necessary attributes")
      # go coerce
      x <- as.xpssFrame(x)
    } 
  }
  
  # attribute cleanup , if a variable is of kind factor
  if(functiontype == "DM" || functiontype == "SB") {
    # find factor variables
    if(is.element("factor",lapply(x,class))){
      var <- names(which(lapply(x,class) == "factor"))
      for(i in 1:length(var)) {
        eval(parse(text =paste("x$",var[[i]]," <- as.character(x$",var[[i]],")",sep="")))  
      }
    }
  }
  
 ############# Attribute - Check ####################
  
  # check if a attribute is missing
  if((length(attributes(x)$FILTER)>0) && (length(attributes(x)$TEMPORARY)>0) && (length(attributes(x)$SPLIT_FILE)>0) && (length(attributes(x)$DO_IF)>0) && (length(attributes(x)$SELECT_IF)>0) && (length(attributes(x)$WEIGHTS)>0)) {
    
    # notifier if object is not xpssFrame
    if(!is.element("xpssFrame",class(x))) {
      message("The actual data isn't a  xpssFrame object. The functionality of some functions is possibly severely limited")
    }
    
    ####### Meta - Check - Datamangement############
    
    # Check whether there are needed attributes in the input-object
    if(!is.null(attributes(x)$FILTER) && functiontype == "DM"){
      ## Filter is set and Function is a Datamanagement Function
      if(attributes(x)$FILTER != FALSE && "DM" %in% class(x)) {
        x <- rbind(x,attributes(x)$FILTERED_DATA)
        attBack <- attributesBackup(x)
        x <- x[order(as.numeric(rownames(x))),]      
        x <- applyAttributes(x, attBack)
        
        attributes(x)$FILTERED_DATA <- NULL
      }
    }
#     
#     else {
#       warning("Meta check couldnt be done. Somewhere an object is handled without needed meta-information")
#     }
    
    ####### Meta - Check - Analyse############
    
    if((attributes(x)$TEMPORARY == TRUE) && (functiontype == "AN")) { 
        attribut_backup <- attributesBackup(x)
        dataname <- get("dataname", parent.frame())
        assign(x=dataname,value=attributes(x)$ORIGIN,envir=.GlobalEnv)
        eval(parse(text = paste0("attributes(", dataname,")$TEMPORARY <- FALSE")), envir = .GlobalEnv)
        eval(parse(text = paste0("attributes(", dataname,")$SELECT_IF <- FALSE")), envir = .GlobalEnv)
        eval(parse(text = paste0("class(", dataname,") <- c('data.frame','xpssFrame')")), envir = .GlobalEnv)
        eval(parse(text = paste0("attributes(", dataname,")$ORIGIN <- NULL")), envir = .GlobalEnv)
    }
#      else {
#       warning("Meta check couldnt be done. Somewhere an object is handled without needed meta-information")
#     }
  } else {
    warning("Essential attributes are missing in the actual object. This problem caused, whether you haven't created a xpssFrame object", call. = F)
  }
    return(x)
}

