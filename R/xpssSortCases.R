#' Sorts data ascending or descending
#'
#' xpssSort reorders the sequence of cases in the dataset based on the values of one or more variables. 
#' 
#' Optionally the sorting can be specified in ascending or descending order for any variable. It is also possible to use combinations of ascending and descending order for different variables.
#'
#' @param x a (non-empty) data.frame, data.table object or input data of class "xpssFrame".  
#' @param variables atomic character or character vector with the name of the variables. Also "rownames" can be used to sort the data. 
#' @param order either "A" for ascending order or "D" for descending order given as character strings. Has to be the same length as variables 
#' @return sorted data frame
#' @author Andreas Wygrabek
#' @seealso \code{\link{sort}} \code{link{order}}
#' @examples
#' data(fromXPSS)
#' xpssSortCases(fromXPSS, variables = c("V4", "V7_1", "V7_2"), order = c("A","D","A"))
#' @export
xpssSortCases <- function(x, variables = NULL, order = "A"){
   
    stopifnot(length(variables) == length(order))  
    stopifnot(order == "A" | order == "D" | order == "UP" | order == "DOWN")
    
    class(x) <- c("xpssFrame","data.frame","DM")
    ####################################################################
    ####################### Meta - Checks ##############################
    ####################################################################
    
    x <- applyMetaCheck(x)
    
    ####################################################################
    ####################################################################
    ####################################################################    
    # Backup of attributes
    attBack <- attributesBackup(x)
    
    if("rownames" %in% variables){
        
        x <- x[order(rownames(x)),]
    } else {
    
      #Do: Sort Ascending If order is 'A' or 'UP', else sort descending
    vec <- ifelse(order == "A" | order == "UP", paste(variables), paste("-",variables,sep =""))
    
    # eval this on data
    eval(parse(text = paste("x <- x[with(x,order(",paste(parse(text = vec),collapse = ','),")),]",sep="")))
    
    }
    # Apply Attributes
    x <- applyAttributes(x, attBack)
    
    ### DeMerge
    ## If Filter is set & Function is a Datamanagement Function
    x <- applyAttributeDemerge(x)
        
    return(x)   
}