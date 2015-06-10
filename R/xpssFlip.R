#' Flips variables
#'
#' R Implementation of the SPSS \code{FLIP} Function. 
#' 
#' @param x a (non-empty) data.frame or input data of class "xpssFrame". 
#' @param variables atomic character or character vector with the names of the variables to flip
#' @param names atomic character with the name of the variable for coloumn names.
#' @return A flipped, respectively transposed xpssFrame object.
#' @author Bastian Wiessner
#' @seealso \code{\link{t}}
#' @export 
#' @examples 
#' data(fromXPSS)
#' xpssFlip(x=fromXPSS,variables=c("V4","V5","V6"),names="V1")

xpssFlip <- function(x, variables = "all", names = NULL){
  #variables <- c("V3","V4","V5","V6")
  #names <- "V6"
  functiontype <- "DM"
  x <- applyMetaCheck(x)
  options(warn = -1)
  if(variables == "all")    {
    # transpose input data
    ta <- t(x) 
    for(i in 1:length(x[[1]])){
   colnames(ta)[i] <- paste("var",i,sep="")
    }
    # coerce to data frame
  } else {
    # check if variables are viable
    for(i in 1:length(variables)) {
      if(!(is.element(variables[[i]],names(x)))) {
        stop("The selected variables has to be in the dataset")
      }  
    }
    ta <- t(x[variables])    
  }    
  # apply attributes
  ta <- as.xpssFrame(ta)
  
  # create var names
  #---------------------
  
  # new names is null
  if(is.null(names)) {
    # default labelling
    for(i in 1:length(x[[1]])){
      names(ta)[i] <- paste("var",i,sep="")
    }
  } else {
    # if names is not null
    # eval the new name variable
    evalVAR <- eval(parse(text=paste("x$",names,sep="")))
    # create a pattern label
    for(i in 1:length(x[[1]])) {
      # if var is numeric
      if(is.numeric(evalVAR)) {
        names(ta)[i] <- paste("K_", evalVAR[i],sep="")  
        # if not numeric
      } else {
        names(ta)[i] <- paste(evalVAR[i],sep="")  
      }      
    }
    # display frequecnies
    hauef <- table(names(ta))
    # count frequencies
    for(i in 1:length(hauef)){
      # count frequencies for 
      id <- which(names(ta) %in% names(hauef[i]))
      for(j in 1:length(id)) {
        names(ta)[id][j] <- paste(names(ta)[id][j],"_",j,sep="")
      }
    }
    # if new name is also in variables
    if(is.element(names,variables)){
      id <- which(!(is.element(variables,names)))
      ta <- ta[id,]      
    }
  }
  options(warn = 0)
  return(ta)  
}