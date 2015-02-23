#' Simple descriptive statistics
#'
#' @description R Implementation of the SPSS Function \code{MEANS}.
#'
#' @details The xpssMeans function displays by default the mean, standard deviation and the amount of observations for a numeric dependent variable. and group counts for a string variable within groups defined by one or more control (independent) variables. Other procedures that display univariate statistics are SUMMARIZE, FREQUENCIES, and DESCRIPTIVES.
#' 
#' \strong{\code{missing:}} 
#' \tabular{rll}{
#' 
#'\tab \code{table} \tab Deletes cases tablewise. 
#'\cr \tab \code{include} \tab Include user-missing values.
#'\cr \tab \code{dependent} \tab Exclude user-missing values for dependent variables only. 
#'} 
#' 
#' \strong{\code{statistics:}} 
#' \tabular{rll}{
#' 
#'\tab \code{anova} \tab ANalysis Of VAriance.  
#'}
#'\strong{\code{cells:}}
#'\tabular{rll}{
#' 
#'\tab \code{all} \tab calculates all following descriptiv functions.
#'\cr \tab \code{count} \tab displays the amount of observations.
#'\cr \tab \code{first} \tab displays the first observation.
#'\cr \tab \code{geometric} \tab displays the geometric mean 
#'\cr \tab \code{harmonic} \tab displays the harmonic mean
#'\cr \tab \code{kurt} \tab calculates the bulge of the variable. 
#'\cr \tab \code{last} \tab displays the last observation. 
#'\cr \tab \code{max} \tab displays the maximum of the variable. 
#'\cr \tab \code{mean} \tab calculates the arithmetic mean, respectively the midpoint of the variable.
#'\cr \tab \code{median} \tab calculates the median of the variable. 
#'\cr \tab \code{min} \tab displays the minimum of the variable. 
#'\cr \tab \code{range} \tab displays the span between the minimum and the maximum value. 
#'\cr \tab \code{sekurtosis} \tab calculates the standrard error of the bulge of the variable. 
#'\cr \tab \code{semean} \tab displays the standard error of the arithmetic mean. 
#'\cr \tab \code{seskewness} \tab calculates the standrard error of the inclination of the variable. 
#'\cr \tab \code{skew} \tab calculates the inclination of the variable. 
#'\cr \tab \code{stddev} \tab  displays the standard deviation of the variable. 
#'\cr \tab \code{sum} \tab calculates the sum of each observation within the variable. 
#'\cr \tab \code{variance} \tab displays the variance.}
#' 
#' @param x a (non-empty) data.frame or input data of class \code{"xpssFrame"}. 
#' @param variables atomic character or character vector with the name of the variables.
#' @param by atomic character or character vector with the name of the variables.
#' @param missing atomic numeric with the name of the missing method. Default is \code{NULL}. Optionally \code{table},\code{dependent} or \code{include} can be chosen. See note for more.
#' @param cells specifies descriptiv statistics for the results. Default is \code{mean}, \code{stddev} and \code{n}. See notes for more.
#' @param statistics specifies a anova or linearity test for each result. Default is \code{NULL}. Optionally \code{anova} or \code{linearty} can be chosen.
#' @author Bastian Wiessner
#' @importFrom data.table data.table
#' @importFrom stringr str_sub str_length
#' @importFrom e1071 kurtosis skewness
#' @examples
#' \dontrun{data(fromXPSS)}
#' @export
#' 
xpssMeans <- function(x,
                      variables=NULL,
                      by=NULL,
                      missing=NULL,
                      cells="default",
                      statistics=NULL){
  
  # do meta check
  
  functiontype <- "AN"
  dataname <- eval(paste0(deparse(substitute(x))), envir = .GlobalEnv)
  x <- applyMetaCheck(x)
  
  ###################################
  
  if(is.null(variables))
  {
    stop("argument variables is missing, no default available")
  }
  for(i in 1:length(variables)) {
  if(!(is.element(variables[[i]],names(x)))) {
    stop("The selected variable has to be in the dataset")
  }
    if(class(x[,variables[i]]) != "numeric"){  
      stop("Variables are not numeric")
    }
  } 
  if(!(is.null(by)))
  {
    for(i in 1:length(by))
    {
      if(class(x[,by[i]]) != "numeric"){  
        stop("by variables are not numeric")
      }
    } 
  }
  
  if(!(is.null(missing)))
  {
    if(!(is.element(missing,c("dependent","include","table")))){
      stop("missing argument is not valid") 
    }
  }

  ######### missing
  #########################################
  
  if(!(is.null(missing))){
    # include
    if(is.element(missing,"include")){
      # select the variables
      temp <- computeValue(x,variables)
      # get the position of the variables
      pos <- which(colnames(temp) %in% variables)
      for(i in 1:length(variables))
      {
        #write the user defined missings back in the actual data
        x[,variables[i]] <- temp[,pos[i]]
      }
      if(!(is.null(by))){
        # select the variables
        temp <- computeValue(x,by)
        # get the position of the variables
        pos <- which(colnames(temp) %in% by)
        for(i in 1:length(by))
        {
          #write the user defined missings back in the actual data
          x[,by[i]] <- temp[,pos[i]]
        } 
      }
    }
    
    #####################################################
    #dependent - include user-missing only for control vars
    if(is.element(missing,"dependent")){
      # select the variables
      temp <- computeValue(x,variables)
      # get the position of the variables
      pos <- which(colnames(temp) %in% variables)
      for(i in 1:length(variables))
      {
        #write the user defined missings back in the actual data
        x[,variables[i]] <- temp[,pos[i]]
      }
    }
    
  }
  
  ###################################
  #
  # data preparation
  
  
  ### create subset
  if(is.null(by)){ # if by is not in
    pos <- which(names(x)%in%variables)  
  } else { # if by is in
    pos <- sort(c(which(names(x)%in%variables),which(names(x)%in%by)))
  }
  # extract and create data.table object
  tinput <- data.table(x[pos])
     
  
  
  # paste action
  
  
  #########################################################################################
  #
  #
  # Cells
  #
  ##
  
  # GMEDIAN NPCT SPCT NPCT(var) SPCT(var) HARMONIC GEOMETRIC ALL
  express <- "list("
  if(is.element("default",cells)){
    express <- paste0(express,"mean=mean(get(variables[[i]]),na.rm=T),sd=sd(get(variables[[i]]),na.rm=T),n=length(na.omit(get(variables[[i]]))),")
  }
  if(is.element("count",cells)){
    express <- paste0(express,"n=length(get(variables[[i]]),na.rm=T),")
  }
  if(is.element("first",cells)){
    express <- paste0(express,"last=tail(get(variables[[i]]), n=1),")
  }
  if(is.element("geometric",cells)){
    express <- paste0(express,"geometric=prod(get(variables[[i]]),na.rm=T)^(1/length(na.omit(get(variables[[i]])))),")
  }
  if(is.element("harmonic",cells)){
    express <- paste0(express,"harmonic=1/mean(1/na.omit(get(variables[[i]]))),")
  }
  if(is.element("kurt",cells)){
    express <- paste0(express,"kurt=kurtosis(get(variables[[i]]),type=2),")
  }
  if(is.element("last",cells)){
    express <- paste0(express,"last=tail(get(variables[[i]]), n=1),")
  }
  if(is.element("max",cells)){
    express <- paste0(express,"max=max(get(variables[[i]]),na.rm=T),")
  }
  if(is.element("mean",cells)){
    express <- paste0(express,"mean=mean(get(variables[[i]]),na.rm=T),")
  }
  if(is.element("median",cells)){
    express <- paste0(express,"median=median(get(variables[[i]]),na.rm=T),")
  }
  if(is.element("min",cells)){
    express <- paste0(express,"min=min(get(variables[[i]]),na.rm=T),")
  }
  if(is.element("n",cells)){
    express <- paste0(express,"n=length(na.omit(get(variables[[i]]))),")
  }
  if(is.element("range",cells)){
    express <- paste0(express,"range=diff(range(get(variables[[i]]),na.rm=T)),")
  }
  if(is.element("sekurt",cells)){
    express <- paste0(express,"sekurt=2*(sqrt((6*length(na.omit(get(variables[[i]])))*(length(na.omit(get(variables[[i]])))-1)) / ((length(na.omit(get(variables[[i]])))-2)*(length(na.omit(get(variables[[i]])))+1)*(length(na.omit(get(variables[[i]])))+3))))* (sqrt(((length(na.omit(get(variables[[i]])))^2 -1)) / ((length(na.omit(get(variables[[i]])))-3)*(length(na.omit(get(variables[[i]])))+5)))),")
  }
  if(is.element("semean",cells)){
    express <- paste0(express,"semean=sd(get(variables[[i]]),na.rm=T)/(sqrt(length(na.omit(get(variables[[i]]))))),")
  }
  if(is.element("skew",cells)){
    express <- paste0(express,"skew=skewness(get(variables[[i]]),type=2),")
  }
  if(is.element("seskew",cells)){
    express <- paste0(express,"seskew=sqrt((6*length(na.omit(get(variables[[i]])))*(length(na.omit(get(variables[[i]])))-1)) / ((length(na.omit(get(variables[[i]])))-2)*(length(na.omit(get(variables[[i]])))+1)*(length(na.omit(get(variables[[i]])))+3))),")
  }
  if(is.element("stddev",cells)){
    express <- paste0(express,"stddev=sd(get(variables[[i]]),na.rm=T),")
  }
  if(is.element("sum",cells)){
    express <- paste0(express,"sum=sum(get(variables[[i]]),na.rm=T),")
  }
  if(is.element("variance",cells)){
    express <- paste0(express,"variance=var(get(variables[[i]]),na.rm=T),")
  }
  if(is.element("all",cells)){
    express <- "list(
                geometric=prod(get(variables[[i]]),na.rm=T)^(1/length(na.omit(get(variables[[i]])))),
                harmonic=(1/mean(1/na.omit(get(variables[[i]])))),
                n=length(na.omit(get(variables[[i]]))),
                kurt=kurtosis(get(variables[[i]]),type=2),
                last=tail(get(variables[[i]]), n=1),
                max=max(get(variables[[i]]),na.rm=T),
                mean=mean(get(variables[[i]]),na.rm=T),
                median=median(get(variables[[i]]),na.rm=T),
                min=min(get(variables[[i]]),na.rm=T),
                range=diff(range(get(variables[[i]]),na.rm=T)),
                sd=sd(get(variables[[i]]),na.rm=T),
                sekurt=2*(sqrt((6*length(na.omit(get(variables[[i]])))*(length(na.omit(get(variables[[i]])))-1)) / ((length(na.omit(get(variables[[i]])))-2)*(length(na.omit(get(variables[[i]])))+1)*(length(na.omit(get(variables[[i]])))+3))))* (sqrt(((length(na.omit(get(variables[[i]])))^2 -1)) / ((length(na.omit(get(variables[[i]])))-3)*(length(na.omit(get(variables[[i]])))+5)))),
                semean=sd(get(variables[[i]]),na.rm=T)/(sqrt(length(na.omit(get(variables[[i]]))))),
                skew=skewness(get(variables[[i]]),type=2),seskew=sqrt((6*length(na.omit(get(variables[[i]])))*(length(na.omit(get(variables[[i]])))-1)) / ((length(na.omit(get(variables[[i]])))-2)*(length(na.omit(get(variables[[i]])))+1)*(length(na.omit(get(variables[[i]])))+3))),
                stddev=sd(get(variables[[i]]),na.rm=T),
                sum=sum(get(variables[[i]]),na.rm=T),
                variance=var(get(variables[[i]]),na.rm=T),"
    

  }
  express <- str_sub(string=express,start=1,end=str_length(express)-1)  
  # eval the pasted expression  
  express <- paste0(express,")")
  ########################### statistics
  # generate output
  out <- list()
  # counter for output
  if(is.null(by)){
    for(i in 1:length(variables)){
      valids <- sum(computeNvalid(x=x,variables=variables[i]))
      misses <- sum(computeNmiss(x=x,variables=variables[i]))
      summ= cbind("valid_obs."=valids, 
            "percent"=paste0((valids/length(x[,variables[i]])*100),"%"),
            "missing obs."=misses,
            "percent"=paste0((misses/length(x[,variables[i]])*100),"%"),
            "overall"=valids+misses,
            "percent"="100%")
      cells <- tinput[,eval(parse(text=express))]
      # generate a anova
      if(!(is.null(statistics))){
        message("anova can not be calculated without control variable")  
      }
      # generate output
      out[[i]] <- list("summary" = summ,
                       "cells"=cells)
                         }
  } else {
    
    k <- 1
    # count amount of vars
    for(i in 1:length(variables)){
      # count amount of control vars
      for(j in 1:length(by)){
        overall <- cbind(x[,variables[i]],x[,by[j]])
        leng <- length(overall[is.na(overall)])
        overallleng <- length(overall[,1])
        summ <-  cbind("valid_obs."=overallleng- leng, 
              "percent"=paste0(((overallleng - leng) /overallleng*100),"%"),
              "missing obs."=leng,
              "percent"=paste0((leng/overallleng*100),"%"),
              "overall"=overallleng,
              "percent"="100%")
        # calculate the cellstatistcs by the pasted express
        cells <- tinput[,eval(parse(text=express)),by=get(by[[j]])]
        # sort ascending
        cells <- cells[order(cells$get)]
        # generate a anova
        if(!(is.null(statistics))){ 
          if(is.element(statistics,"anova")){
            model <- lm(x[,variables[i]] ~ x[,by[j]])
            stats <- aov(model)
            attributes(stats$terms)$term.labels <- attributes(x[,variables[i]])$variable.label
            stats <- summary(stats)            
            eta <- c("eta²"=summary.lm(model)$r.squared,"eta"=sqrt(summary.lm(model)$r.squared))
            out[[k]] <- list("summary" = summ, "cells"=cells,"anova"=stats,"measure_of_association"=eta)
            k <- k+1
          }  
        } else{
          out[[k]] <- list("summary" = summ, "cells"=cells)
          k <- k+1
        }        
        # generate output
        
      }
    }
  }
  names(out) <- variables
  out <- noquote(out)
  return(out)
}
