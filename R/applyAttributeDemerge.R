#' Merges Attributdata 
#'
#' Attribut merge function
#'
#' Helper Function for DeMerging Attributedatasets
#' 
#' @param x a (non-empty) data.frame or input data of class \code{"xpssFrame"}. 
#' @return Output is the input data demerged on the basis of the attributes of the actual dataset.
#' @author Bastian Wiessner
#' @keywords internal

applyAttributeDemerge <- function(x) {
  
  # if function is data management function
    if(!is.null(attributes(x)$FILTER)){ # This line corresponds to applyMetaCheck
      if(attributes(x)$FILTER != FALSE && "DM" %in% class(x) == T) {
          
        # dont work due wrong usage
        # x <- xpssFilter(x, attributes(x)$FILTER)
        # does not fit with xpssFilter(x,variable = NULL, filtervalue = 1)
        # change of position vector  
        pos <-  which(eval(parse(text=paste("x$",attributes(x)$FILTER,sep=""))))
        
        ## rest of the body is equal to xpssFilter
                
        attr(x, "FILTERED_DATA") <- x[setdiff(1:nrow(x),pos),]
        
        # ATTRIBUTES BACKUP
        attBack <- attributesBackup(x)
        # -
        
        x <- x[pos,]

        # APPLY ATTRIBUTES
        x <- applyAttributes(x, attBack)
      }
    } 
    
  return(x)
}

