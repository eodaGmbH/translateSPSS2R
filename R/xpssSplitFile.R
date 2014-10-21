#' Splits the data.frame into pieces by a factor
#'
#' R Implementation of the SPSS \code{filter} argument
#'
#' @usage xpssSplitFile(x, by = NULL, type = "seperated", attachSplit = FALSE)
#' @param x a (non-empty) data.frame, data.table object or input data of class \code{"xpssFrame"}. 
#' @param by the factor variable which splits the data frame
#' @param type the results from an analysis function applied on a splitted data frame can be presented either in a seperated or an layered design
#' @param attachSplit Should the splitted data be attached to the object as an attribute?
#' @return The function return the input data with modified attributes
#' @author Andreas Wygrabek
#' @seealso \code{\link{xpssDoIf}} \code{\link{xpssFilterOff}} \code{\link{xpssSelectIf}} 
#' \code{\link{xpssTemporary}} 
#' @examples 
#' data(fromXPSS)
#' xpssSplitFile(fromXPSS, "V6", "layered", attachSplit = FALSE)
#' @export
xpssSplitFile <- function(x, by = NULL, type = "seperated", attachSplit = FALSE){
    
    # Exception
    if(!is.element(type, c("seperated", "layered"))){
        stop("type has to be 'seperated' or 'layered'")
    }
    
    # The attribute is changed
    # attributes(substitute(x))$SPLIT_FILE <<- paste(type, "by", by)
    eval(parse(text = paste0("attributes(", substitute(x),")$SPLIT_FILE <- ", paste("'",type, 'by', by,"'"))), envir = .GlobalEnv)
    
    # attachSplit: If TRUE, an attribute is created with the splitted data. 
    if(attachSplit == TRUE){
    attr(x,"splitted_data") <<- split(x, as.factor(eval(parse(text = paste0(as.character(substitute(x)),"$",by)))))
    # Label the elements of splitted_data-List
    names(attributes(x)$splitted_data) <- names(sort(attributes(eval(parse(text = paste0(as.character(substitute(x)),"$",by))))$value.labels))
    }
    
    print("Data split is activated")
}



