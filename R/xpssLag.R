#' Moves cases of variables
#'
#' xpssLag shifts the dataset forward or backward by a given number of observations.
#' 
#' Creates shifted data dependent upon in which direction the data got moved. \cr A positive indicator refers a shift to the right side, a negative indicator refers a shift of the data to the left side. Empty cases get filled with NA.
#'
#' @usage xpssLag(x, move = 0, value = NA)
#' @param x a (non-empty) data.frame, data.table object or input data of class "xpssFrame". 
#' @param move Either a positive or negative integer value that defines the cases to move. The algebraic sign indicates the direction. 
#' @param value The value that replaces the skipped cases.
#' @return Output is the shifted, respectively laged input vector. \cr Length of the new laged vector is identical with the length of the input vector.
#' @author Andreas Wygrabek
#' @examples 
#' foo <- matrix(data = c(1:12), 
#'              nrow = 6,
#'              ncol = 2,
#'              dimnames = list(c(1,2,3,4,5,6),c("A","B")))
#' foo <- as.data.frame(foo)
#' xpssLag(foo$B, move = 2, value = NA)
#' @export
xpssLag <- function(x, move = 0, value = NA){ 
    
  
  stopifnot(class(x) != "numeric")
  
    dfcheck <- deparse(substitute(x))
    dfcheck <- strsplit(dfcheck, "\\$")[[1]][1]
  
  absMove <- abs(move)   
    
    if (move < 0 ){       
        OBJ <- c(tail(x,-absMove),rep(value,absMove))
    }   
    else if (move > 0 ){        
        OBJ <- c(rep(value,absMove), head(x,-absMove))
    }   
    else {       
        OBJ <- x
       }   
    return(OBJ)
}
