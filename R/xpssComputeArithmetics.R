#' Computes the absolute value.
#' 
#' 
#' Helper Function for xpssCompute. R Implementation of the SPSS \code{ABS} Function. 
#'
#' 
#' @usage computeAbs(x) 
#' @param x an atomic numeric or numeric vector or numeric matrix.
#' @return Returns a vector with the absolute values of the input data.
#' @author Bastian Wiessner
#' @seealso \code{\link{abs}}
#' @examples
#' data(fromXPSS)
#' # atomic numeric input
#' x1 <- fromXPSS$V5
#' xpssCompute(x = x1,fun = "computeAbs")
#' # numeric vector input
#' x2 <- c(fromXPSS$V7_1,fromXPSS$V7_2,fromXPSS$V5)
#' xpssCompute(x = x2,fun = "computeAbs")
#' # numeric matrix input
#' x3 <- rbind(fromXPSS$V7_2,fromXPSS$V5,fromXPSS$V6)
#' xpssCompute(x = x3,fun = "computeAbs")
#' @keywords internal
#' @export
#' 
computeAbs <- function(x){

  #exceptions
    if(!(is.numeric(x))){
    stop("input data has to be numeric")
  }
  
  if(is.atomic(x) || is.vector(x) || is.matrix(x)){
    out <- base::abs(x) 
  } else{
    stop("wrong input format")
  }
  return(out)
}

#' Computes the arc-sine.
#'
#'
#' Helper Function for xpssCompute. R Implementation of the SPSS \code{ARSIN} Function.
#' 
#' 
#' @usage computeArsin(x)
#' @param x an atomic numeric or numeric vector or numeric matrix.
#' @return returns a vector with the arc-sine values of the input data.
#' @author Bastian Wiessner
#' @seealso \code{\link{asin}}
#' @examples 
#' # atomic numeric input with values between -1 and 1
#' x1 <- rnorm(n=20,mean=0,sd=0.25)
#' xpssCompute(x=x1,fun="computeArsin")
#' # numeric vector input with values between -1 and 1
#' x2 <- c(rnorm(n=20,mean=0,sd=0.25),rnorm(n=20,mean=0,sd=0.35))
#' xpssCompute(x=x2,fun="computeArsin")
#' # numeric matrix input with values between -1 and 1
#' x3 <- rbind(rnorm(n=20,mean=0,sd=0.25),rnorm(n=20,mean=0,sd=0.35),rnorm(n=20,mean=0,sd=0.5))
#' xpssCompute(x=x3,fun="computeArsin")
#' @keywords internal
#' @export

computeArsin <- function(x){
  if(!(is.numeric(x))){
    stop("input data has to be numeric")
  }
  if(is.atomic(x) || is.vector(x) || is.matrix(x)){
    out <- asin(x)  
  }else{
    stop("wrong input format")
  }
  return(out)
}

#' Computes the arc-tan.
#' 
#' 
#' Helper Function for xpssCompute. R Implementation of the SPSS \code{ARTAN} Function.
#'
#'
#' @usage computeArtan(x) 
#' @param x an atomic numeric or numeric vector or numeric matrix.
#' @return returns a numeric, numeric vector or matrix with the arc-tan values of the input data.
#' @author Bastian Wiessner
#' @seealso \code{\link{atan}}
#' @examples 
#' data(fromXPSS)
#' # atomic numeric input
#' x1 <- fromXPSS$V5
#' xpssCompute(x = x1,fun = "computeArtan")
#' # numeric vector input
#' x2 <- c(fromXPSS$V7_1,fromXPSS$V7_2,fromXPSS$V5)
#' xpssCompute(x = x2,fun = "computeArtan")
#' # numeric matrix input
#' x3 <- rbind(fromXPSS$V7_2,fromXPSS$V5,fromXPSS$V6)
#' xpssCompute(x = x3,fun = "computeArtan")
#' @keywords internal
#' @export

computeArtan <- function(x){
  
  if(!(is.numeric(x))){
    stop("input data has to be numeric")
  }
  if(is.atomic(x) || is.vector(x) || is.matrix(x)){
    out <- atan(x)  
  }else{
    stop("wrong input format")
  }
  return(out)
}


#' Computes the cosinus.
#' 
#' 
#' Helper Function for xpssCompute. R Implementation of the SPSS \code{COS} Function.
#'
#'
#' @usage computeCos(x) 
#' @param x an atomic numeric or numeric vector or numeric matrix.
#' @return returns a numeric, numeric vector or matrix with the cosinus values of the input data.
#' @author Bastian Wiessner
#' @seealso \code{\link{cos}}
#' @examples 
#' data(fromXPSS)
#' # atomic numeric input
#' x1 <- fromXPSS$V5
#' xpssCompute(x = x1,fun = "computeCos")
#' # numeric vector input
#' x2 <- c(fromXPSS$V7_1,fromXPSS$V7_2,fromXPSS$V5)
#' xpssCompute(x = x2,fun = "computeCos")
#' # numeric matrix input
#' x3 <- rbind(fromXPSS$V7_2,fromXPSS$V5,fromXPSS$V6)
#' xpssCompute(x = x3,fun = "computeCos")
#' @keywords internal
#' @export

computeCos <- function(x){
  
  if(!(is.numeric(x))){
    stop("input data has to be numeric")
  }
  if(is.atomic(x) || is.vector(x) || is.matrix(x)){
    out <- cos(x)  
  }else{
    stop("wrong input format")
  }
  return(out)
}


#' Computes the exponential
#' 
#' 
#' Helper Function for xpssCompute. R Implementation of the SPSS \code{EXP} Function.
#'
#'
#' @usage computeExp(x) 
#' @param x an atomic numeric or numeric vector or numeric matrix.
#' @return returns a numeric, numeric vector or matrix with the expontential values of the input data.
#' @author Bastian Wiessner
#' @seealso \code{\link{exp}}
#' @examples 
#' data(fromXPSS)
#' # atomic numeric input
#' x1 <- fromXPSS$V5
#' xpssCompute(x = x1,fun = "computeExp")
#' # numeric vector input
#' x2 <- c(fromXPSS$V7_1,fromXPSS$V7_2,fromXPSS$V5)
#' xpssCompute(x = x2,fun = "computeExp")
#' # numeric matrix input
#' x3 <- rbind(fromXPSS$V7_2,fromXPSS$V5,fromXPSS$V6)
#' xpssCompute(x = x3,fun = "computeExp")
#' @keywords internal
#' @export

computeExp <- function(x){
  
  if(!(is.numeric(x))){
    stop("input data has to be numeric")
  }
  if(is.atomic(x) || is.vector(x) || is.matrix(x)){
    out <- exp(x)  
  }else{
    stop("wrong input format")
  }
  return(out)
}



#' Computes the logarithm base 10
#' 
#' 
#' Helper Function for xpssCompute. R Implementation of the SPSS \code{LG10} Function.
#'
#'
#' @usage computeLg10(x) 
#' @param x an atomic numeric or numeric vector or numeric matrix.
#' @return returns a numeric, numeric vector or matrix with the logarithm base 10 values of the input data.
#' @author Bastian Wiessner
#' @seealso \code{\link{log}}
#' @examples 
#' data(fromXPSS)
#' # atomic numeric input
#' x1 <- fromXPSS$V5
#' xpssCompute(x = x1,fun = "computeLg10")
#' # numeric vector input
#' x2 <- c(fromXPSS$V7_1,fromXPSS$V7_2,fromXPSS$V5)
#' xpssCompute(x = x2,fun = "computeLg10")
#' # numeric matrix input
#' x3 <- rbind(fromXPSS$V7_2,fromXPSS$V5,fromXPSS$V6)
#' xpssCompute(x = x3,fun = "computeLg10")
#' @keywords internal
#' @export

computeLg10 <- function(x){
  
  if(!(is.numeric(x))){
    stop("input data has to be numeric")
  }
  if(is.atomic(x) || is.vector(x) || is.matrix(x)){
    out <- log10(x)  
  }else{
    stop("wrong input format")
  }
  return(out)
}


#' Computes the logarithm naturalis
#' 
#' 
#' Helper Function for xpssCompute. R Implementation of the SPSS \code{LN} Function.
#'
#'
#' @usage computeLn(x) 
#' @param x an atomic numeric or numeric vector or numeric matrix.
#' @return returns a numeric, numeric vector or matrix with the logarithm naturalis values of the input data.
#' @author Bastian Wiessner
#' @seealso \code{\link{log}}
#' @examples 
#' data(fromXPSS)
#' # atomic numeric input
#' x1 <- fromXPSS$V5
#' xpssCompute(x = x1,fun = "computeLn")
#' # numeric vector input
#' x2 <- c(fromXPSS$V7_1,fromXPSS$V7_2,fromXPSS$V5)
#' xpssCompute(x = x2,fun = "computeLn")
#' # numeric matrix input
#' x3 <- rbind(fromXPSS$V7_2,fromXPSS$V5,fromXPSS$V6)
#' xpssCompute(x = x3,fun = "computeLn")
#' @keywords internal
#' @export

computeLn <- function(x){
  
  if(!(is.numeric(x))){
    stop("input data has to be numeric")
  }
  if(is.atomic(x) || is.vector(x) || is.matrix(x)){
    out <- log(x)  
  }else{
    stop("wrong input format")
  }
  return(out)
}


#' Computes the logarithm of the gamma function
#' 
#' 
#' Helper Function for xpssCompute. R Implementation of the SPSS \code{LNGAMMA} Function.
#'
#'
#' @usage computeLngamma(x) 
#' @param x an atomic numeric or numeric vector or numeric matrix.
#' @return returns a numeric, numeric vector or matrix with the logarithm of the gamma function values of the input data.
#' @author Bastian Wiessner
#' @seealso \code{\link{lgamma}}
#' @examples 
#' data(fromXPSS)
#' # atomic numeric input
#' x1 <- fromXPSS$V5
#' xpssCompute(x = x1,fun = "computeLngamma")
#' # numeric vector input
#' x2 <- c(fromXPSS$V7_1,fromXPSS$V7_2,fromXPSS$V5)
#' xpssCompute(x = x2,fun = "computeLngamma")
#' # numeric matrix input
#' x3 <- rbind(fromXPSS$V7_2,fromXPSS$V5,fromXPSS$V6)
#' xpssCompute(x = x3,fun = "computeLngamma")
#' @keywords internal
#' @export

computeLngamma <- function(x){
  
  if(!(is.numeric(x))){
    stop("input data has to be numeric")
  }
  if(is.atomic(x) || is.vector(x) || is.matrix(x)){
    out <- lgamma(x)  
  }else{
    stop("wrong input format")
  }
  return(out)
}



#' Returns the remainder of a division.
#' 
#' 
#' Helper Function for xpssCompute. R Implementation of the SPSS \code{MOD} Function.
#'
#'
#' @usage computeMod(x,modulus = NULL)
#' @param x an atomic numeric or numeric vector or numeric matrix.
#' @param modulus atomic numeric \code{x} is divided by.
#' @return returns a numeric vector including the remainder of \code{x} by \code{modulus}.
#' @author Bastian Wiessner
#' @seealso \code{\link{\%\%}}
#' @keywords internal
#' @examples 
#' # atomic numeric input
#' x1 <- seq(from = 1,to = 20,by = 1.5)
#' xpssCompute(x = x1,fun = "computeMod", modulus = 2)
#' # numeric vector input
#' x2 <- c(seq(from = 1,to = 20,by = 1.5),seq(from = 1,to = 20,by = 1.25))
#' xpssCompute(x = x2,fun = "computeMod", modulus = 3)
#' # numeric matrix input
#' x3 <- rbind(seq(from = 1,to = 10,by = 1),seq(from = 1,to = 20,by = 2),seq(from = 1,to = 30,by = 3))
#' xpssCompute(x = x3,fun = "computeMod", modulus = 4)
#' @export

computeMod <- function(x,modulus = NULL){

  if(is.null(modulus)){
    stop("argument modulus is missing")
  } else{
    if(is.numeric(modulus)){
      if(!(is.numeric(x))){
        stop("input data has to be numeric")
      }
      if(is.atomic(x)){
        out <-  x %% modulus
      }
      if(is.matrix(x)){
        out <- x
        for(i in 1:ncol(x)) {
          # amount of observations
          for(j in 1:nrow(x)) {
            # fill data with first object
            out[,i][j] <- x[,i][j] %% modulus
          }       
        }  
      }
      if(is.vector(x)){
        out <-  x %% modulus
      }
    }
    else{
      stop("modulus has to be numeric")
    }
  }
  return(out)
}

#' Rounds values.
#' 
#' 
#' Helper Function for xpssCompute. R Implementation of the SPSS \code{RND} Function.
#'
#'
#' @usage computeRnd(x, digits=2)
#' @param x a numeric or complex vector.
#' @param digits atomic numeric argument, indicating the number of decimal places.
#' @return returns the rounded values of \code{x} with the specified number of decimal places.
#' @author Bastian Wiessner
#' @seealso \code{\link{round}}
#' @keywords internal
#' @examples 
#' # atomic numeric input
#' x1 <- seq(from = 1,to = 20,by = 1.5)
#' xpssCompute(x = x1,fun = "computeRnd", digits = 2)
#' # numeric vector input
#' x2 <- c(seq(from = 1,to = 20,by = 1.5),seq(from = 1,to = 20,by = 1.25))
#' xpssCompute(x = x2,fun = "computeRnd", digits = 3)
#' # numeric matrix input
#' x3 <- rbind(seq(from = 1,to = 10,by = 1),seq(from = 1,to = 20,by = 2),seq(from = 1,to = 30,by = 3))
#' xpssCompute(x = x3,fun = "computeRnd", digits = 1)
#' @export
#' 

computeRnd <- function(x,digits=2){
  out <- round(x,digits=digits)
  return(out)
}

#' Computes the sine function
#' 
#' 
#' Helper Function for xpssCompute. R Implementation of the SPSS \code{SIN} Function.
#'
#'
#' @usage computeSin(x) 
#' @param x an atomic numeric or numeric vector or numeric matrix.
#' @return returns a numeric, numeric vector or matrix with the sine values of the input data.
#' @author Bastian Wiessner
#' @seealso \code{\link{sin}}
#' @examples 
#' data(fromXPSS)
#' # atomic numeric input
#' x1 <- fromXPSS$V5
#' xpssCompute(x = x1,fun = "computeSin")
#' # numeric vector input
#' x2 <- c(fromXPSS$V7_1,fromXPSS$V7_2,fromXPSS$V5)
#' xpssCompute(x = x2,fun = "computeSin")
#' # numeric matrix input
#' x3 <- rbind(fromXPSS$V7_2,fromXPSS$V5,fromXPSS$V6)
#' xpssCompute(x = x3,fun = "computeSin")
#' @keywords internal
#' @export

computeSin <- function(x){
  
  if(!(is.numeric(x))){
    stop("input data has to be numeric")
  }
  if(is.atomic(x) || is.vector(x) || is.matrix(x)){
    out <- sin(x)  
  }else{
    stop("wrong input format")
  }
  return(out)
}

#' Computes the square root
#' 
#' 
#' Helper Function for xpssCompute. R Implementation of the SPSS \code{SQRT} Function.
#'
#'
#' @usage computeSqrt(x) 
#' @param x an atomic numeric or numeric vector or numeric matrix.
#' @return returns a numeric, numeric vector or matrix with the square root values of the input data.
#' @author Bastian Wiessner
#' @seealso \code{\link{sqrt}}
#' @examples 
#' data(fromXPSS)
#' # atomic numeric input
#' x1 <- fromXPSS$V5
#' xpssCompute(x = x1,fun = "computeSqrt")
#' # numeric vector input
#' x2 <- c(fromXPSS$V7_1,fromXPSS$V7_2,fromXPSS$V5)
#' xpssCompute(x = x2,fun = "computeSqrt")
#' # numeric matrix input
#' x3 <- rbind(fromXPSS$V7_2,fromXPSS$V5,fromXPSS$V6)
#' xpssCompute(x = x3,fun = "computeSqrt")
#' @keywords internal
#' @export

computeSqrt <- function(x){
  
  if(!(is.numeric(x))){
    stop("input data has to be numeric")
  }
  if(is.atomic(x) || is.vector(x) || is.matrix(x)){
    out <- sqrt(x)  
  }else{
    stop("wrong input format")
  }
  return(out)
}