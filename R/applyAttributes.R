#' Apply stored attributes 
#'
#' Applies attributes stored by attributesBackup
#'
#' @usage applyAttributes(x, attributesToApply = NULL)
#' @param x a data.frame, data.table object or input data of class xpssFrame. 
#' @param attributesToApply to applied attributes
#' @return Object with attributes from backup
#' @author Andreas Wygrabek


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
      # second exception needed for dataframes
        if((!is.null(attributes(x[,i])$varname)) && !is.null(names(attributesToApply$local))){
        attributes(x[,i]) <- attributesToApply$local[[which(names(attributesToApply$local) == attributes(x[,i])$varname)]]
        } else if(names(x)[i] %in% names(attributesToApply$local)){
            attributes(x[,i]) <- attributesToApply$local[[which(names(attributesToApply$local) %in% names(x)[i])]] 
            ### needed condition for xpssFrame
            } else if(colnames(x)[i] %in% colnames(attributesToApply$local)){
              attributes(x[,i]) <- attributesToApply$local[,which(colnames(attributesToApply$local) %in% colnames(x)[i])]
            }
    }
    
    return(x)
    
}
