#' Defines missing values for variables.
#'
#' Defines values as missing and replaces them with \code{NA}. Position and Value are stored in the attributes of the specific variables.
#' 
#' xpssMissingValues sepcifies values for missing data for the selected variables. Those variables which match the terms of beeing a missing data get treated as \code{NA}. Variables which contain \code{NA}, receive in most cases a special treatment in data management, case selection, and descriptive, respectively inductive statistics. 
#' Defines missing values for variables.
#'
#' Defines values as missing and replace them with \code{NA}. Position and Value are stored in the attributes of the specific variables.
#' 
#' xpssMissingValues sepcifies values for missing data for the selected variables. Those variables which match the terms of beeing a missing data get treated as \code{NA}. Variables which contain \code{NA}, receive in most cases a special treatment in data management, case selection, and descriptive, respectively inductive statistics. 
#'\cr \cr User-missing values and system-missing values get treated as exactly one kind of missing data. The only difference in those missing values are that system missings get automatically assigned by the program when no legal value can be produced (e.g. character input at a numeric varibale, failed datatransformation) and user-defined missings, which are missing user data (e.g. the respondent forgot to answer, or skipped the question). \cr \cr Common is that this empty spaces are filled with \emph{-9 till -999} (for e.g. refusal to respond, inability to respond, Non-contact). \cr \cr
#'
#' The \code{as.missing} statement indicates the handling of values which are matched by the as.missing statement, \cr
#' \tabular{rlll}{
#' 
#' \tab \code{singlevalues} \tab  specifies atomic missings \cr 
#' \tab \code{range}  \tab defines a vector from x to y, which sets all matched values to NA.} \cr 
#'
#' \strong{NOTE:} The special arguments \code{lo} and  \code{hi} can be used to determine the lowest and highest value of a numeric value. \cr 
#'
#' @usage xpssMissingValues(x, variables = NULL, as.missing = list(range = c(from=NULL,to=NULL),singlevalues = NULL), append = FALSE)
#' 
#' @param x a (non-empty) data.frame, data.table object or input data of class \code{"xpssFrame"}. 
#' @param variables one or more variables given as character. 
#' @param as.missing numeric list of values to define missing values.
#' @param append logical indicator, which specifies if the existing missings should get overwritten or not.
#' @return a data frame with \code{NAs} located at the position where the specified values in as.missing used to be. In the attributes of the object the position and the value itself is stored. 
#' @author Andreas Wygrabek
#' @examples 
#' data(fromXPSS)
#' temp <- xpssMissingValues(fromXPSS, variable = "V6", as.missing = list(range=c(from="lo",to=45)))
#' @export
xpssMissingValues <- function(x, variables = NULL, as.missing = list(range = c(from=NULL,to=NULL), singlevalues = NULL), append = FALSE){
  
  missing_range <- list()
  class(x) <- c("xpssFrame","data.frame","DM")
  functiontype <- "DM"
    ####################################################################
    ####################### Meta - Checks ##############################
    ####################################################################
  
  x <- applyMetaCheck(x)
  
    ####################################################################
    ####################################################################
    ####################################################################
    
    if(!is.element(TRUE, class(x) == "xpssFrame")){
        stop("Input has to be from class xpssFrame")
    } else {
        
      LEN <- length(variables)
        for(i in 1:length(variables)){
        VAR <- paste("x$",variables[i], sep = "")
        evalVAR <- eval(parse(text = paste(VAR)))
                
        
        #abfangen der special characters
        if(!is.null(as.missing$range)) {
          if("lo" %in% as.missing$range) {
            pos <- which(as.missing$range %in%  "lo" | as.missing$range %in% "lowest")  
            as.missing$range[[pos]] <- min(evalVAR,na.rm=T)
          }
          if("hi" %in% as.missing$range) {
            pos <- which(as.missing$range %in% "hi" |as.missing$range %in% "highest")  
            as.missing$range[[pos]] <- max(evalVAR,na.rm=T)
          }
        }
        
        ## 'Detektieren von Missing Ranges
        
        if(!is.null(as.missing$range)) {
          missing_min <- as.numeric(as.missing$range[[1]])
          missing_max <- as.numeric(as.missing$range[[2]])
          for(j in 1:length(evalVAR)){
            if((is.na(evalVAR[j]) == FALSE) && (missing_min <= evalVAR[j]) && (evalVAR[j] <= missing_max))
            {
              missing_range[[j]] <- evalVAR[j]
            }
          }
        }
        missings <- c(as.missing$singlevalues,unlist(missing_range))
              

        # Ein Attribut defined.MIS schreiben, wenn dieses noch nicht vorhanden war
        if(eval(parse(text = paste("is.null(attributes(x$",variables[i],")$defined.MIS) | (!is.null(attributes(x$",variables[i],")$defined.MIS) & !append)", sep = "")))){
          
                
          #### Falls Input eine Liste
          if(length(as.missing$singlevalues)> 0 && length(as.missing$range)> 0) {
            eval(parse(text=paste("attr(x$",variables[i],",'defined.MIS') <- list(values = as.missing$singlevalues[[i]],range= as.missing$range)", sep ="")))
            
          } else if((length(as.missing$singlevalues)> 0)  && (is.null(as.missing$range))) {
           
            eval(parse(text=paste("attr(x$",variables[i],",'defined.MIS') <- list(values= as.missing$singlevalues[[i]])", sep ="")))
           
          } else {
            eval(parse(text=paste("attr(x$",variables[i],",'defined.MIS') <- list(range= as.missing$range)", sep ="")))
          }
          
        } else if(eval(parse(text = paste("!is.null(attributes(x$",variables[i],")$defined.MIS) & append", sep = "")))){ # Anhängen der Werte an defined.MIS wenn append = TRUE
          
          
          if(length(as.missing$singlevalues)> 0 && length(as.missing$range)> 0) {
            eval(parse(text=paste("attr(x$",variables[i],",'defined.MIS') <- c(attributes(x$",variables[i],")$defined.MIS,list(values=unique(as.missing$singlevalues[[i]]), range= as.missing$range))", sep ="")))
            
          } else if((length(as.missing$singlevalues)> 0)  && (is.null(as.missing$range))) {
                    
                    eval(parse(text=paste("attr(x$",variables[i],",'defined.MIS') <- c(attributes(x$",variables[i],")$defined.MIS,list(values=unique(as.missing$singlevalues[[i]])))", sep ="")))
                    
          } else {
            eval(parse(text=paste("attr(x$",variables[i],",'defined.MIS') <- c(attributes(x$",variables[i],")$defined.MIS,list(range=unique(as.missing$range)))", sep ="")))
          }
          
        } 
        
        
        
        # Anweisungen, wenn noch keine defined.MIS vorliegen
        if(is.null(eval(parse(text = paste("attributes(x$",variables[i],")$MIS", sep = ""))))){ # Wenn NULL == TRUE wird der Missing Frame angelegt 

            POS <- which(is.element(evalVAR,missings))
            VAL <- evalVAR[which(is.element(evalVAR,missings))]
            newMIS <- cbind(POS,VAL)
            
            eval(parse(text = paste("attr(x","$",variables[i],",'MIS') <- newMIS", sep = "")))
  
         
            # Anweisungen, wenn bereits Missing Werte vorliegen
            } else if(append){
                
                eval(parse(text = paste("OBJ <- attributes(x$",variables[i],")$MIS", sep = "")))
                
                POS <- which(is.element(evalVAR,missings))
                VAL <- evalVAR[which(is.element(evalVAR,missings))]
                newMIS <- cbind(POS,VAL)
                MIS <- eval(parse(text=paste("rbind(attributes(x$",variables[i],")$MIS, newMIS)", sep = "")))
                MIS <- unique(MIS)
                MIS <- MIS[order(MIS[,1]),]
           
                eval(parse(text = paste("attr(x$",variables[i],",'MIS') <- MIS", sep = "")))
                
            } else { # Wenn defined.MIS vorliegen, diese aber überschrieben werden sollen
                
                # Zunächst werden die derzeit als MISSING definierten Werte wieder an den Datensatz geschrieben. 
                eval(parse(text = paste("x$",variables[i],"[attributes(x$",variables[i],")$MIS[,1]] <- attributes(x$",variables[i],")$MIS[,2]", sep ="")))
                evalVAR <- eval(parse(text = paste(VAR)))
                
  
                POS <- which(is.element(evalVAR,missings))
                VAL <- evalVAR[which(is.element(evalVAR,missings))]
                newMIS <- cbind(POS,VAL)
                  
                eval(parse(text = paste("attr(x","$",variables[i],",'MIS') <- newMIS", sep = "")))
            }
}
}
  # Umwandeln der Werte in NAs
  
   for(i in 1:length(variables)){
       
       eval(parse(text = paste("x$",variables[i],"[attributes(x$",variables[i],")$MIS[,1]] <- NA", sep = "")))
       
   }


### DeMerge
## Filter is set & Function is a Datamanagement Function
x <- applyAttributeDemerge(x)
  
  return(x)
}


