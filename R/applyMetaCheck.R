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

   # Check whether there are needed attributes in the input-object
if(!is.null(attributes(x)$FILTER)){
    ## Filter is set & Function is a Datamanagement Function
    if(attributes(x)$FILTER != FALSE && "DM" %in% class(x)) {
        x <- rbind(x,attributes(x)$FILTERED_DATA)
        attBack <- attributesBackup(x)
        x <- x[order(as.numeric(rownames(x))),]      
        x <- applyAttributes(x, attBack)
        
        attributes(x)$FILTERED_DATA <- NULL
        }
    } else {
        warning("Meta check couldn´t be done. Somewhere an object is handled without needed meta-information")
}
    return(x)
}

