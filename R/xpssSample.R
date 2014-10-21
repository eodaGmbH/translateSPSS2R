#' samples the a dataset
#'
#' Takes a sample from a xpssFrame, data frame or matrix
#'
#' sample takes a sample of the specified size from the elements of x using either with or without replacement.
#' 
#' \code{pct} specifies a percentage of value which should be kept, allowed value range is from 1 to 0.01.
#' \code{n} indicates the amount of values to keep. \code{n} has to be lower then \code{from}.
#' \code{from} determines the basis for n. \code{from} has to be higher then \code{n}.
#'
#' @param x a (non-empty) data.frame, data.table object or input data of class \code{"xpssFrame"}. 
#' @param pct Percentage to keep
#' @param n Number of cases to keep
#' @param from Basis for n
#' @return a subset of the actual dataset. The subset get specified by pct or n. 
#' @author Andreas Wygrabek
#' @seealso \code{\link{sample}}
#' @examples
#' data(fromXPSS)
#' xpssSample(fromXPSS, pct = 0.5)
#' @export
xpssSample <- function(x, pct = NULL, n = NULL, from = NULL){
    
    stopifnot(n<from)
  
    stopifnot(is.data.frame(x) | is.data.table(x) | class(x) == "xpssFrame")
    
      class(x) <- c("xpssFrame","data.frame","DM")  

    
    ####################################################################
    ####################### Meta - Checks ##############################
    ####################################################################
    x <- applyMetaCheck(x)
    ####################################################################
    ####################################################################
    ####################################################################
    
    
    attr_backup <- attributesBackup(x)
    
    if(!is.null(pct) && !is.null(n))
    {
      stop("it is only possible to use pct or n, but not both arguments at the same time.")
    }
    if(!is.null(from)){
      if(length(x[[1]]) < from)
      {
        stop("from is longer than the dataset, chose a basis value which is smaller or has a equal length of the dataset")
      } 
    }
    if(!is.null(from)){
      if(length(x[[1]]) <= n)
      {
        stop("n is longer than the dataset, chose a basis value which has a smaller length of the dataset")
      } 
    }
      # Wenn pct
      if(!is.null(pct)){
        if(is.data.frame(x) | is.matrix(x)){
          
          sampVec <- 1:nrow(x)
          
          sampVec <- sample(sampVec,round(length(sampVec)*pct, 0))
          
          out <- x[sampVec,]
          
        } else {
          
          sampVec <- 1:length(x)
          
          sampVec <- sample(sampVec,round(length(sampVec)*pct, 0))
          
          out <- x[sampVec]
          
        }
        # ---- Wenn n
      } else if(is.null(from)) {
        if(is.data.frame(x) | is.matrix(x)){
          
          sampVec <- 1:nrow(x)
          
          sampVec <- sample(sampVec,n, 0)
          
          out <- x[sampVec,]
          
        } else {
          
          sampVec <- 1:length(x)
          
          sampVec <- sample(sampVec,n, 0)
          
          out <- x[sampVec]
          
        }   
        # ----- Wenn n und from
      } else if(!is.null(n) & !is.null(from) & if(is.data.frame(x) | is.matrix(x)){nrow(x)>from} else {length(x)>from}){
        if(is.data.frame(x) | is.matrix(x)){
          
          sampVec <- 1:nrow(x)
          
          sampVec <- sample(sampVec[c(1:from)],round(length(sampVec[c(1:from)])*(n/from),0), 0)
          
          out <- x[sampVec,]
          
          } else {
          
          sampVec <- 1:length(x)
          
          sampVec <- sample(sampVec[c(1:from)],round(length(sampVec[c(1:from)])*(n/from),0), 0)
          
          out <- x[sampVec]
          
        } 
        
      }
      # ---- Wenn from > x
      else {
        if(is.data.frame(x) | is.matrix(x)){
          
          sampVec <- 1:nrow(x)
          
          sampVec <- sample(sampVec,round(length(sampVec)*(n/from),0), 0)
          
          out <- x[sampVec,]
          
        } else {
          
          sampVec <- 1:length(x)
          
          sampVec <- sample(sampVec,round(length(sampVec)*(n/from),0), 0)
          
          out <- x[sampVec]
        }   
      }  
    
    for(i in 1:length(out)) {
      attributes(out[[i]]) <- attr_backup$local[[i]]  
  
      na_pos <- which(rownames(out[i]) %in% attributes(out[[i]])$MIS)
      value_pos <- which(attributes(out[[i]])$MIS %in% rownames(out[i]))
      attr_temp <- attributes(out[[i]])$MIS
      
      if(length(na_pos>0)){ 
        attributes(out[[i]])$MIS <- NULL
        attributes(out[[i]])$MIS <- data.frame("POS" = na_pos, "VAL"=attr_temp[,2][value_pos])
      }
      
    } 
    
    
    ### DeMerge
    ## Filter is set & Function is a Datamanagement Function
    
    if(attributes(out)$FILTER != FALSE) {
      pos <- which(!(eval(parse(text = paste("out$",(attributes(x)$FILTER),sep="")))))
      attributes(out)$FILTERED_DATA <- out[pos,]
      pos <- which(eval(parse(text = paste("out$",(attributes(x)$FILTER),sep=""))))
      out <-  out[pos,]      
    } else { ## dunno if needed
      x <- applyAttributeDemerge(x)
    }

    return(out)
  }
    
   
