
# MAX Function ------------------------------------------------------------


#' Computes the Maxima
#'
#' Helper Function for xpssCompute. R Implementation of the SPSS \code{MAX} Function. 
#' 
#' @usage computeMax (x,...)
#'
#' @param x atomic numeric or numeric vector or numeric matrix
#' @param ... further arguments passed to or from other methods.
#'
#' @return Numeric. Returns the maximum value of its arguments that have valid values. 
#' If the data contains missing values, it is possible to specify an na remove command, this is \code{na.rm}. The default for na.rm is \code{na.rm=F}, if the value get changed to \code{na.rm=T} every existing missing value get omitted.
#' @author Bastian Wiessner
#' @seealso \code{\link{max}}
#' @keywords internal
#' @examples
#' data(fromXPSS)
#' # atomic numeric input
#' x1 <- fromXPSS$V5
#' xpssCompute(x = x1,fun = "computeMax")
#' # numeric vector input
#' x2 <- c(fromXPSS$V7_1,fromXPSS$V7_2,fromXPSS$V5)
#' xpssCompute(x = x2,fun = "computeMax")
#' # numeric matrix input, output is NA
#' x3 <- rbind(fromXPSS$V7_2,fromXPSS$V4)
#' xpssCompute(x = x3,fun = "computeMax")
#' # numeric matrix input, output is MAX
#' x4 <- rbind(fromXPSS$V7_2,fromXPSS$V4)
#' xpssCompute(x = x3,fun = "computeMax", na.rm = T)
#' @export
#' 

computeMax <- function(x,...){
  out <- x
  if(is.matrix(x)){
    for(i in 1:nrow(x)){
      out[i,] <- max(x[i,],...)  
    }
    out <-  out[,1]  
  }  
  if(is.vector(x) & is.atomic(x)){
    message("NOTE: computeMax needs atleast two variables seperated by coloumn to work correctly")
    out <- x
  }
  if(is.atomic(x) & (!(is.vector(x))) & (!(is.matrix(x)))){
    message("NOTE: computeMax needs atleast two variables seperated by coloumn to work correctly")
    out <- x
  }
  return(out)
}



# MEAN Function -----------------------------------------------------------


#' Atithmetic Mean
#'
#'  Helper Function for xpssCompute. R Implementation of the SPSS \code{MEAN} Function. 
#'
#' @usage computeMean (x,...)
#'
#' @param x Numeric or numeric vector
#' @param ... further arguments passed to or from other methods.
#' @return Numeric. Returns the arithmetic mean of its arguments that have valid, nonmissing values. 
#' If the data contains missing values, it is possible to specify an na remove command, this is \code{na.rm}. The default for na.rm is \code{na.rm=F}, if the value get changed to \code{na.rm=T} every existing missing value get omitted.
#' @author Bastian Wiessner
#' @seealso \code{\link{mean}}
#' @keywords internal
#' @examples
#' data(fromXPSS)
#' # atomic numeric input
#' x1 <- fromXPSS$V5
#' xpssCompute(x = x1,fun = "computeMean")
#' # numeric vector input
#' x2 <- c(fromXPSS$V7_1,fromXPSS$V7_2,fromXPSS$V5)
#' # optional with na remove
#' xpssCompute(x = x2,fun = "computeMean",na.rm=T)
#' # numeric matrix input, output is NA
#' x3 <- rbind(fromXPSS$V7_2,fromXPSS$V4)
#' xpssCompute(x = x3,fun = "computeMax")
#' # numeric matrix input, output is Mean
#' x4 <- rbind(fromXPSS$V7_2,fromXPSS$V4)
#' xpssCompute(x = x3,fun = "computeMax", na.rm = T)
#' @export
#' 

computeMean <- function(x,...){
  if(is.matrix(x)){
    out <- x
    for(i in 1:nrow(x)){
      out[i,] <- mean(x[i,],...)  
    }
    out <-  out[,1]
  }
  if(is.vector(x) & is.atomic(x)){
    message("NOTE: computeMean needs atleast two variables seperated by coloumn to work correctly")
    out <- x
  }
  if(is.atomic(x) & (!(is.vector(x))) & (!(is.matrix(x)))){
    message("NOTE: computeMean needs atleast two variables seperated by coloumn to work correctly")
    out <- x
  }
  
  return(out)
}


# MEDIAN Function ---------------------------------------------------------



#' Median Value
#'
#'  Helper Function for xpssCompute. R Implementation of the SPSS \code{MEDIAN} Function. 
#'
#' @usage computeMedian (x,...)
#'
#' @param x Numeric or numeric vector
#' @param ... further arguments passed to or from other methods.
#'
#' @return Numeric. Returns the median (50th percentile) of its arguments that have valid, nonmissing values.  
#' If the data contains missing values, it is possible to specify an na remove command, this is \code{na.rm}. The default for na.rm is \code{na.rm=F}, if the value get changed to \code{na.rm=T} every existing missing value get omitted.
#' @author Bastian Wiessner
#' @seealso \code{\link{median}}
#' @keywords internal
#' @examples
#' data(fromXPSS)
#' # atomic numeric input
#' x1 <- fromXPSS$V5
#' xpssCompute(x = x1,fun = "computeMedian")
#' # numeric vector input
#' x2 <- c(fromXPSS$V7_1,fromXPSS$V7_2,fromXPSS$V5)
#' # optional with na remove
#' xpssCompute(x = x2,fun = "computeMedian",na.rm=T)
#' # numeric matrix input
#' x3 <- rbind(fromXPSS$V7_2,fromXPSS$V5,fromXPSS$V6)
#' xpssCompute(x = x3,fun = "computeMedian")
#' @export

computeMedian <- function(x,...){
  if(is.matrix(x)){
    out <- x
    for(i in 1:nrow(x)){
      out[i,] <- median(x[i,],...)  
    }
    out <-  out[,1]
  }
  if(is.vector(x) & is.atomic(x)){
    message("NOTE: computeMedian needs atleast two variables seperated by coloumn to work correctly")
    out <- x
  }
  if(is.atomic(x) & (!(is.vector(x))) & (!(is.matrix(x)))){
    message("NOTE: computeMedian needs atleast two variables seperated by coloumn to work correctly")
    out <- x
  }
  return(out)
}


# MIN Function ------------------------------------------------------------



#' Minima
#'
#'  Helper Function for xpssCompute. R Implementation of the SPSS \code{MIN} Function. 
#'
#' @usage computeMin (x,...)
#'
#' @param x Numeric or numeric vector
#' @param ... further arguments passed to or from other methods.
#'
#' @return Numeric or string. Returns the minimum value of its arguments that have valid, nonmissing values. 
#' If the data contains missing values, it is possible to specify an na remove command, this is \code{na.rm}. The default for na.rm is \code{na.rm=F}, if the value get changed to \code{na.rm=T} every existing missing value get omitted.
#' @author Bastian Wiessner
#' @seealso \code{\link{min}}
#' @keywords internal
#' @examples
#' data(fromXPSS)
#' # atomic numeric input
#' x1 <- fromXPSS$V5
#' xpssCompute(x = x1,fun = "computeMin")
#' # numeric vector input
#' x2 <- c(fromXPSS$V7_1,fromXPSS$V7_2,fromXPSS$V5)
#' # optional with na remove
#' xpssCompute(x = x2,fun = "computeMin",na.rm=T)
#' # numeric matrix input
#' x3 <- rbind(fromXPSS$V7_2,fromXPSS$V5,fromXPSS$V6)
#' xpssCompute(x = x3,fun = "computeMin")
#' @export

computeMin <- function(x,...){
  if(is.matrix(x)){
    out <- x
    for(i in 1:nrow(x)){
      out[i,] <- min(x[i,],...)  
    }
    out <-  out[,1]
  }
  if(is.vector(x) & is.atomic(x)){
    message("NOTE: computeMin needs atleast two variables seperated by coloumn to work correctly")
    out <- x
  }
  if(is.atomic(x) & (!(is.vector(x))) & (!(is.matrix(x)))){
    message("NOTE: computeMin needs atleast two variables seperated by coloumn to work correctly")
    out <- x
  }
  return(out)
}



# SD Function -------------------------------------------------------------



#' Standard Deviation
#'
#'  Helper Function for xpssCompute. R Implementation of the SPSS \code{SD} Function. 
#'
#' @usage computeSd (x,...)
#'
#' @param x Numeric or numeric vector
#' @param ... further arguments passed to or from other methods.
#'
#' @return Numeric. Returns the standard deviation of its arguments that have valid, nonmissing values.  
#' If the data contains missing values, it is possible to specify an na remove command, this is \code{na.rm}. The default for na.rm is \code{na.rm=F}, if the value get changed to \code{na.rm=T} every existing missing value get omitted.
#' @author Bastian Wiessner
#' @seealso \code{\link{sd}}
#' @keywords internal
#' @examples
#' data(fromXPSS)
#' # atomic numeric input
#' x1 <- fromXPSS$V5
#' xpssCompute(x = x1,fun = "computeSd")
#' # numeric vector input
#' x2 <- c(fromXPSS$V7_1,fromXPSS$V7_2,fromXPSS$V5)
#' # optional with na remove
#' xpssCompute(x = x2,fun = "computeSd",na.rm=T)
#' # numeric matrix input
#' x3 <- rbind(fromXPSS$V7_2,fromXPSS$V5,fromXPSS$V6)
#' xpssCompute(x = x3,fun = "computeSd")
#' @export

computeSd <- function(x,...){
  if(is.matrix(x)){
    out <- x
    for(i in 1:nrow(x)){
      out[i,] <- sd(x[i,],...)  
    }
    out <-  out[,1]
  }
  if(is.vector(x) & is.atomic(x)){
    message("NOTE: computeSd needs atleast two variables seperated by coloumn to work correctly")
    out <- x
  }
  if(is.atomic(x) & (!(is.vector(x))) & (!(is.matrix(x)))){
    message("NOTE: computeSd needs atleast two variables seperated by coloumn to work correctly")
    out <- x
  }
  return(out)
}



# VARIANCE Function -------------------------------------------------------



#' Variance
#'
#'  Helper Function for xpssCompute. R Implementation of the SPSS \code{VARIANCE} Function. 
#'
#' @usage computeVariance (x,...)
#'
#' @param x Numeric or numeric vector
#' @param ... further arguments passed to or from other methods.
#'
#' @return Numeric. Returns the variance of its arguments that have valid values.
#' If the data contains missing values, it is possible to specify an na remove command, this is \code{na.rm}. The default for na.rm is \code{na.rm=F}, if the value get changed to \code{na.rm=T} every existing missing value get omitted.
#' @author Bastian Wiessner
#' @seealso \code{\link{var}}
#' @keywords internal
#' @examples
#' data(fromXPSS)
#' # atomic numeric input
#' x1 <- fromXPSS$V5
#' xpssCompute(x = x1,fun = "computeVariance")
#' # numeric vector input
#' x2 <- c(fromXPSS$V7_1,fromXPSS$V7_2,fromXPSS$V5)
#' # optional with na remove
#' xpssCompute(x = x2,fun = "computeVariance",na.rm=T)
#' # numeric matrix input
#' x3 <- rbind(fromXPSS$V7_2,fromXPSS$V5,fromXPSS$V6)
#' xpssCompute(x = x3,fun = "computeVariance")
#' @export

computeVariance <- function(x,...){
  if(is.matrix(x)){
    out <- x
    for(i in 1:nrow(x)){
      out[i,] <-  var(x[i,],...)  
    }
    out <-  out[,1]
  }
  if(is.vector(x) & is.atomic(x)){
    message("NOTE: computeVariance needs atleast two variables seperated by coloumn to work correctly")
    out <- x
  }
  if(is.atomic(x) & (!(is.vector(x))) & (!(is.matrix(x)))){
    message("NOTE: computeVariance needs atleast two variables seperated by coloumn to work correctly")
    out <- x
  }
  return(out)
}
