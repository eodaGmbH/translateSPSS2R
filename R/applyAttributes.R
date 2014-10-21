#' Apply stored attributes 
#'
#' Applies attributes stored by attributesBackup
#'
#' @param x a data.frame, data.table object or input data of class xpssFrame. 
#' @param attributesToApply to applied attributes
#' @return Object with attributes from backup
#' @author Andreas Wygrabek
#' @examples
#' data(fromXPSS)
#' myAtt <- attributesBackup(fromXPSS)
#' NEW_fromXPSS <- fromXPSS[order(fromXPSS[,5]),]
#' applyAttributes(NEW_fromXPSS, myAtt)
#' @export
applyAttributes <- function(x, attributesToApply = NULL){
    
    
    ### Exceptions
    if(!"xpssAttributes" %in% class(attributesToApply))
    {
        stop("Attributes have to be stored in an object of class xpssAttributes")
    }
    

#         if(nrow(x) != length(attributesToApply$global$row.names)){
#             stop("To apply the rownames from the former attributes the data sets have to be of same length")
#         }
    
    ####
    
    # ----
    # Take rownames from data-input
    backRN <- rownames(x)

    attributesToApply[["global"]]$row.names <- backRN
    #----
    
    # Apply global attributes
    attributes(x) <- attributesToApply[["global"]] 
    

    # Apply local attributes
    for(i in 1:ncol(x)){
        if(!is.null(attributes(x[,i])$varname)){
        attributes(x[,i]) <- attributesToApply$local[[which(names(attributesToApply$local) == attributes(x[,i])$varname)]]
        } else if(names(x)[i] %in% names(attributesToApply$local)){
            attributes(x[,i]) <- attributesToApply$local[[which(names(attributesToApply$local) %in% names(x)[i])]] 
        }
    }
    
    return(x)
    
}
