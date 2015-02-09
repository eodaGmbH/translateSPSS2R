#' Displays whether a variable contains missing values
#'
#' R Implementation of the SPSS \code{Missing} Function.
#'
#' Performs a missing value operation. \code{miss} displays system- and user-defined missing values of variables.
#' The different missing types will be inspected without checking if they are system-; or user-defined missings.
#' 
#' \code{variable} input is limited to only one variable.
#' 
#' @usage miss (x, variable = NULL)
#' @param x a (non-empty) data.frame, data.table object or input data of class "xpssFrame". 
#' @param variable atomic character with the name of the variable.
#' @return atomic logical with the length of the data. Returns \code{TRUE},
#' if the variable contains a system- or user-defined missing value.
#' @author Bastian Wiessner
#' @seealso Related Functions \code{\link{nmiss}} , \code{\link{nvalid}} , \code{\link{sysmis}} ,\code{\link{value}}
#' @keywords internal
#' @examples
#' # load data
#' data(fromXPSS)
#' 
#' # display missings in variable V7_2
#' miss(fromXPSS, variable="V7_2")
#' @export

miss <- function(x, variable = NULL){
  
  
  ####################################################################
  ####################### Meta - Checks ##############################
  ####################################################################
  
  functiontype <- "DM"
  x <- applyMetaCheck(x)
  
  ####################################################################
  ####################################################################
  ####################################################################
  
  
  LOGMAT <- matrix(0, ncol = length(variable), nrow = nrow(x))
  
  NUM <- sapply(x[,variable], function(x){
    is.numeric(x)})
  CHA <- sapply(x[,variable], function(x){
    is.character(x)})
  FAC <- sapply(x[,variable], function(x){
    is.factor(x)})
  
  if(length(variable) == 1)
  {
    LOGMAT <- is.na(x[,variable])
    OUT <- LOGMAT  
  }
  else{
    stop("Function is specified for only one Variable")
  }
  
  return(OUT)
}


#' Displays the amount of missing values in variables.
#'
#' R implementation of the SPSS \code{Nmiss} function
#'
#' Performs a missing value operation. \code{nmiss} displays the amount of system- and user-defined missing values of the variables. \cr User-defined and system-defind missings values get handled as one type of missing value.
#'
#' @usage nmiss(x, variables = NULL)
#' @param x a (non-empty) data.frame, data.table object or input data of class \code{"xpssFrame"}. 
#' @param variables atomic character or character vector with the names of the variables.
#' @return atomic numeric with the length of the data. Returns the amount of
#' system- or user-defined missing values of the variables.
#' @author Bastian Wiessner
#' @seealso Related Functions \code{\link{missing}} , \code{\link{nvalid}} , \code{\link{sysmis}} ,\code{\link{value}}
#' @keywords internal
#' @examples
#' # load data
#' data(fromXPSS)
#' 
#' # display the amount of user- and system-defined missing values in variable V6 and V7_2
#' nmiss(fromXPSS, variables=c("V6","V7_2"))
#' @export


nmiss <- function(x, variables = NULL){
  
  ####################################################################
  ####################### Meta - Checks ##############################
  ####################################################################
  
  functiontype <- "DM"
  x <- applyMetaCheck(x)
  
  ####################################################################
  ####################################################################
  ####################################################################
  
  
  LOGMAT <- matrix(0, ncol = length(variables), nrow = nrow(x))
  
  NUM <- sapply(x[,variables], function(x){
    is.numeric(x)})
  CHA <- sapply(x[,variables], function(x){
    is.character(x)})
  FAC <- sapply(x[,variables], function(x){
    is.factor(x)})
  
  for(i in 1:length(variables)){
    
    attribut_indicator <- attr(x[,variables[i]],"MIS")[,1] 
    na_indicator <- which(is.na(x[,variables[i]]))
    pos <- c(attribut_indicator, na_indicator)
    
    x[,variables[[i]]][pos] <- NA
    
    if(length(variables) == 1)
    {          
      LOGMAT[,i] <- (is.na(x[,variables[i]]))
      OUT <- rowSums(LOGMAT)
      
    } else if(sum(NUM) == length(variables) | sum(CHA) == length(variables) | sum(FAC) == length(variables)){
      LOGMAT[,i] <- is.na(x[,variables[i]]) 
      OUT <- rowSums(LOGMAT)
    } else {
      stop("Variables are not from the same type")
    }
    
  }
  return(OUT)
}

#' Displays the amount of valid values in variables.
#'
#' R Implementation of the SPSS \code{Nvalid} function.
#'
#' Performs a missing value operation. As opposite of \code{nmiss}, \code{nvalid} visualizies only the valid values of integer vectors or character vectors.
#'
#' \code{variables} variables have to be same type.
#'
#' @usage nvalid (x, variables = NULL)
#'
#' @param x a (non-empty) data.frame, data.table object or input data of class \code{"xpssFrame"}. 
#' @param variables atomic character or character vector with the names of the variables.
#' @return atomic numeric with the length of the data. Returns the amount of valid values of the variables.
#' @author Bastian Wiessner
#' @seealso Related Functions \code{\link{missing}} , \code{\link{nmiss}} , \code{\link{sysmis}} ,\code{\link{value}}
#' @keywords internal
#' @examples
#' 
#' # load data
#' data(fromXPSS)
#' 
#' # display the amount of valid observations in variable V6 and V7_2
#' nvalid(fromXPSS, variables=c("V6","V7_2"))
#' @export

nvalid <- function(x, variables = NULL){
  
  ####################################################################
  ####################### Meta - Checks ##############################
  ####################################################################
  
  functiontype <- "DM"
  x <- applyMetaCheck(x)
  
  ####################################################################
  ####################################################################
  ####################################################################
  
  
  LOGMAT <- matrix(0, ncol = length(variables), nrow = nrow(x))
  
  NUM <- sapply(x[,variables], function(x){
    is.numeric(x)})
  CHA <- sapply(x[,variables], function(x){
    is.character(x)})
  FAC <- sapply(x[,variables], function(x){
    is.factor(x)})
  
  for(i in 1:length(variables)){
    
    attribut_indicator <- attr(x[,variables[i]],"MIS")[,1] 
    na_indicator <- which(is.na(x[,variables[i]]))
    pos <- c(attribut_indicator, na_indicator)
    
    x[,variables[[i]]][pos] <- NA
    
    if(length(variables) == 1)
    {
      LOGMAT[,i] <- !is.na(x[,variables[i]])
      OUT <- rowSums(LOGMAT)
      
    } else if(sum(NUM) == length(variables) | sum(CHA) == length(variables) | sum(FAC) == length(variables)){
      LOGMAT[,i] <- !is.na(x[,variables[i]]) 
      OUT <- rowSums(LOGMAT)
    } else {
      stop("Variables are not from the same type")
    }
    
  }
  return(OUT)
}

#' Displays if a variable contains system-missing values
#'
#' R implementation of the SPSS \code{Sysmis} Function
#'
#' Performs a missing value operation. Displays only the amount of system missing values of the variable instead of looking after all missing values only system-missings get inspected.
#'
#' \code{variable} is limited to one variable.
#'
#' @usage sysmis (x, variable = NULL)
#'
#' @param x a (non-empty) data.frame, data.table object or input data of class "xpssFrame". 
#' @param variable atomic character with the name of the variable.
#' @return atomic numeric with the length of the data. Returns the amount of system missing values of the variable.
#' @author Bastian Wiessner
#' @seealso Related Functions \code{\link{missing}} , \code{\link{nmiss}} , \code{\link{nvalid}} ,\code{\link{value}}
#' @keywords internal
#' @examples
#' 
#' # load data
#' data(fromXPSS)
#' 
#' # locate sysmis in variable V7_2
#' sysmis(fromXPSS, variable="V7_2")
#' @export

sysmis <- function(x, variable = NULL){
  
  functiontype  <- "SB"
  x <- applyMetaCheck(x)
  
  if(length(variable) == 1)
  {
    pos <- attr(x[,variable],"MIS")[,1]
    x[,variable][pos] <- attr(x[,variable],"MIS")[,2]
    logvec <- is.na(x[,variable])
  }
  else{
    stop("Function is specified for only one Variable")
  }
  
  return(logvec)
}


#' Displays the value of the variable cases.
#'
#' R implementation of the SPSS \code{Value} function.
#'
#' Performs a missing value operation. Adds the user-defined missing values of the variables in the existing dataset.
#'
#' \code{variables} variables have to be same type.
#'
#' @usage value(x, variables = NULL)
#' @param x a (non-empty) data.frame, data.table object or input data of class "xpssFrame". 
#' @param variables atomic character or character vector with the names of the variables.
#' @return A "xpssFrame" object or (data frame) with implemented user-defined missing values. 
#' 
#' \strong{NOTE:} The whole dataset gets returned! 
#' 
#' @author Bastian Wiessner
#' @seealso Related Functions \code{\link{missing}} , \code{\link{nmiss}} , \code{\link{nvalid}} , \code{\link{sysmis}}
#' @keywords internal
#' @examples
#' # load data
#' data(fromXPSS)
#' 
#' # add user defined values for variable V7_2  
#' value(fromXPSS, variables="V7_2")
#'
#' # add user defined values for variable V6 and V7_2   
#' temp <- value(fromXPSS, variables=c("V6","V7_2"))
#' 
#' #' # add user defined values for variable V6 and V7_2   
#' temp <- value(fromXPSS)
#' 
#' @export

value <- function(x, variables = NULL){
  
  ####################################################################
  ####################### Meta - Checks ##############################
  ####################################################################
  
  functiontype <- "DM"
  x <- applyMetaCheck(x)
  
  ####################################################################
  ####################################################################
  ####################################################################
  
  
  attBack <- attributesBackup(x)
  
  if(is.null(variables)) {
    for(i in 1:length(x[1,])) {
      attribut_indicator <- attributes(x[[i]])$MIS[,1] 
      x[[i]][attribut_indicator] <- attributes(x[[i]])$MIS[,2] 
    }
  } else {
    
    for(i in 1:length(variables)){
      attribut_indicator <- attr(x[,variables[i]],"MIS")[,1] 
      x[,variables[i]][attribut_indicator] <- attr(x[,variables[i]],"MIS")[,2] 
    }
  }
  
  x <- applyAttributes(x=x,attributesToApply=attBack)
  
  return(x)
}