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
#'\tab \code{anova} \tab calculates sumsquare, degrees of freedom, meansquare, f-value and significance level.
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
#' @param statistics specifies a anova or linearity test for each result. Default is \code{NULL}. Optionally \code{anova }can be chosen.
#' @author Bastian Wiessner
#' @importFrom data.table data.table setkeyv setnames setorderv setorder set as.data.table
#' @importFrom stringr str_sub str_length str_split str_split_fixed
#' @importFrom e1071 kurtosis skewness
#' @examples
#' 
#' # one var
#' xpssMeans(x=fromXPSS,variables="V3")
#' 
#' # two vars
#' xpssMeans(x=fromXPSS,variables=c("V3","V4"))
#' 
#' # two vars + one controlvar
#' xpssMeans(x=fromXPSS,variables=c("V3","V6"),by="V4")
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
  options(warn=-1)
  ###################################
  
  if(is.null(variables))
  {
    stop("argument variables is missing, no default available")
  }
  for(i in 1:length(variables)) {
    if(!(is.element(variables[[i]],names(x)))) {
      stop("The selected variables have to be in the dataset")
    }
    if(class(x[,variables[i]]) != "numeric"){  
      stop("Variables are not numeric")
    }
  } 
  if(!(is.null(by)))
  {
    for(i in 1:length(by)){
      if(!(is.element(by[[i]],names(x)))) {
        stop("The selected by-variables have to be in the dataset")
      }
      if(class(x[,by[i]]) != "numeric"){  
        stop("by variables are not numeric")
      }
    } 
  }
  
  if(!(is.null(missing)))
  {
    if(any(!(is.element(missing,c("dependent","include","table"))))){
      stop("missing argument is not valid") 
    }
  }
  
  if(!(is.null(statistics)))
  {
    if(!(is.element(statistics,c("anova")))){
      stop("statistics argument is not valid") 
    }
  }
  if(!(is.null(cells))){
    if(any(!(is.element(cells,c("default","all", "count", "first","geometric","harmonic","kurt","last","max","mean","median","min","range","sekurtosis","semean","seskewness","skew","stddev","sum","variance"))))){
      stop("cells argument is not valid")
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
      if(length(variables)>1){
        pos <- which(colnames(temp) %in% variables)
        for(i in 1:length(variables)) {
          #write the user defined missings back in the actual data
          x[,variables[i]] <- temp[,pos[i]]
        }  
      } else{
        x[,variables] <- temp
      }      
      if(!(is.null(by))){
        # select the variables
        temp <- computeValue(x,by)
        # get the position of the variables
        if(length(by)>1){
          pos <- which(colnames(temp) %in% by)
          for(i in 1:length(by))  {
            #write the user defined missings back in the actual data
            x[,by[i]] <- temp[,pos[i]]
          }  
        } else{
          x[,by] <- temp
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
      for(i in 1:length(variables))    {
        #write the user defined missings back in the actual data
        x[,variables[i]] <- temp[,pos[i]]
      }
    }
    
  }
  ###################################
  #
  # data preparation
  # if split file is true
  if(unique(attributes(x)$SPLIT_FILE != FALSE)){
    # split the variablenames
    splitter <- unlist(str_split(attributes(x)$SPLIT_FILE,pattern=" "))
    # exclude the the last statement
    splitter <- splitter[1:length(splitter)-1]
    splitnames <- unlist(str_split(splitter,pattern=","))
    # combine variables and split vars
    vars <- c(variables,splitnames)
    ### create subset
    if(is.null(by)){ # if by is not in
      # which vars are in the dataset
      pos <- which(names(x)%in%vars)  
    } else {
      # which vars and byvars are in the dataset
      pos <- sort(c(which(names(x)%in%vars),which(names(x)%in%by)))
    }
    # create data.table object
    tinput <- data.table(x[pos])  
    # combine the split vars for data.table operations
    splitter <- paste(splitter,collapse=",")
  } else{
    if(is.null(by)){ # if by is not in
      # which vars are in the dataset
      pos <- which(names(x)%in%variables)  
    } else {
      # which vars and byvars are in the dataset
      pos <- sort(c(which(names(x)%in%variables),which(names(x)%in%by)))
    }
    # create data.table object
    tinput <- data.table(x[pos])  
  }
  
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
    express <- paste0(express,"n=length(na.omit(get(variables[[i]]))),")
  }
  if(is.element("first",cells)){
    express <- paste0(express,"first=head(get(variables[[i]]), n=1),")
  }
  if(is.element("geometric",cells)){
    express <- paste0(express,"geometric=prod(get(variables[[i]]),na.rm=T)^(1/length(na.omit(get(variables[[i]])))),")
  }
  if(is.element("harmonic",cells)){
    express <- paste0(express,"harmonic=1/mean(1/na.omit(get(variables[[i]]))),")
  }
  if(is.element("kurt",cells)){
    express <- paste0(express,"kurt=if(length(na.omit(get(variables[[i]])))>=4) kurtosis(get(variables[[i]]),na.rm=T,type=2) else 0,")
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
    express <- paste0(express,"skew=if(length(na.omit(get(variables[[i]])))>=3) skewness(get(variables[[i]]),na.rm=T,type=2) else 0,")
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
    express <- "list(geometric=prod(get(variables[[i]]),na.rm=T)^(1/length(na.omit(get(variables[[i]])))),harmonic=(1/mean(1/na.omit(get(variables[[i]])))),n=length(na.omit(get(variables[[i]]))),kurt=if(length(na.omit(get(variables[[i]])))>=4) kurtosis(get(variables[[i]]),na.rm=T,type=2) else 0,first=head(get(variables[[i]]), n=1),last=tail(get(variables[[i]]), n=1),max=max(get(variables[[i]]),na.rm=T),   mean=mean(get(variables[[i]]),na.rm=T),  median=median(get(variables[[i]]),na.rm=T),  min=min(get(variables[[i]]),na.rm=T),  range=diff(range(get(variables[[i]]),na.rm=T)),sd=sd(get(variables[[i]]),na.rm=T),sekurt=2*(sqrt((6*length(na.omit(get(variables[[i]])))*(length(na.omit(get(variables[[i]])))-1)) / ((length(na.omit(get(variables[[i]])))-2)*(length(na.omit(get(variables[[i]])))+1)*(length(na.omit(get(variables[[i]])))+3))))* (sqrt(((length(na.omit(get(variables[[i]])))^2 -1)) / ((length(na.omit(get(variables[[i]])))-3)*(length(na.omit(get(variables[[i]])))+5)))),semean=sd(get(variables[[i]]),na.rm=T)/(sqrt(length(na.omit(get(variables[[i]]))))), skew=if(length(na.omit(get(variables[[i]])))>=3) skewness(get(variables[[i]]),na.rm=T,type=2) else 0,seskew=sqrt((6*length(na.omit(get(variables[[i]])))*(length(na.omit(get(variables[[i]])))-1)) / ((length(na.omit(get(variables[[i]])))-2)*(length(na.omit(get(variables[[i]])))+1)*(length(na.omit(get(variables[[i]])))+3))), stddev=sd(get(variables[[i]]),na.rm=T), sum=sum(get(variables[[i]]),na.rm=T),  variance=var(get(variables[[i]]),na.rm=T),"}
  express <- str_sub(string=express,start=1,end=str_length(express)-1)  
  # eval the pasted expression  
  express <- paste0(express,")")
  ########################### statistics
  # generate output
  result <- outs <-  out <- variances <- summs <- list()
  # counter for output
  if(is.null(by)){
    if(any(attributes(x)$SPLIT_FILE == F)){
      for(i in 1:length(variables)){
        
        #calculate valid obs
        valids <- sum(computeNvalid(x=x,variables=variables[i]))
        # calculate missing obs
        misses <- sum(computeNmiss(x=x,variables=variables[i]))
        # paste em
        summ= cbind("valid_obs."=valids, 
                    "percent"=paste0((valids/length(x[,variables[i]])*100),"%"),
                    "missing obs."=misses,
                    "percent"=paste0((misses/length(x[,variables[i]])*100),"%"),
                    "overall"=valids+misses,
                    "percent"="100%")
        # calc statistics
        cells <- tinput[,eval(parse(text=express))]
        # generate a anova
        if(!(is.null(statistics))){
          message("anova can not be calculated without control variable")  
        }
        # generate output
        result[[i]] <- list("summary" = summ,
                            "cells"=cells)
        names(result)[[i]] <- variables[[i]]
      }
    } else{
      for(i in 1:length(variables)){
        
        # create empty data.frame for summary
        overall <-  data.frame()
        # create empty list 
        temp <- list()
        # get the group indicators
        pos <- unique(x[paste(unlist(str_split(splitter,pattern=",")))])
        # order indicators
        pos <- as.data.table(pos)
        setkeyv(x=pos,cols=colnames(pos))
        
        # indicate the group
        if(is.null(nrow(pos))){
          for(m in 1:length(pos)){  
            if(any(is.na(pos))){ #& k == 1)
              temp[[m]] <- x[which(is.na(x[,splitter])),]
            } else{
              temp[[m]] <- na.omit(x[eval(parse(text=paste(paste0("x$",splitter),"==",pos[m],collapse=" & "))),])
            }
            # get the row values
            rows <- eval(parse(text=paste0("temp[[m]]$",variables[[i]])))
            # combine them
            overall <- rbind(overall,rows)
          }
        } else{
          for(m in 1:nrow(pos)){  
            if(any(is.na(pos))){
              naexpress <- paste(paste0("x$",names(pos)),"==",pos[m,],collapse=" & ")
              navec <- str_split_fixed(string=naexpress,pattern="&",n=length(pos))
              napos <- grep(pattern="NA",x=navec)
              navec[napos] <-  paste0("is.na(",str_sub(string=navec[napos],start=0,end=str_locate_all(pattern="==",navec[napos])[[1]][1]-1),")")
              temp[[m]] <- x[eval(parse(text=paste(navec, collapse=" & "))),]  
            }else{
              temp[[m]] <- x[eval(parse(text=paste(paste0("x$",names(pos)),"==",pos[m,],collapse=" & "))),]  
            }
            if(length(which(!is.na(str_locate(rownames(temp[[m]]),pattern="NA")[,1])))>=1){
              temp[[m]] <- temp[[m]][-which(!is.na(str_locate(rownames(temp[[m]]),pattern="NA")[,1])),]  
            } 
            overall <- rbind(overall,length(eval(parse(text=paste0("temp[[m]]$",variables[[i]])))))
          }
        }
        
        ################################################
        # create a matrix with 6 colouns to prepare the output
        summ <- matrix(ncol=6)
        for(m in 1:nrow(overall)){
          # get missings
          if(any(is.na(eval(parse(text=paste0("temp[[m]]$",variables[[i]])))))){
            leng <- length(which(is.na(eval(parse(text=paste0("temp[[m]]$",variables[[i]]))))))
          } else{
            leng <- 0
          }          
          # get amount of obs
          overallleng <- overall[m,]
          # calculate the missing coloumns
          tempsumm <-  cbind("valid_obs."=overallleng- leng, 
                             "percent"=paste0(((overallleng - leng) /overallleng*100),"%"),
                             "missing obs."=leng,
                             "percent"=paste0((leng/overallleng*100),"%"),
                             "overall"=overallleng,
                             "percent"="100%")
          if(m==1){
            # create summ
            summ <- tempsumm
          } else{
            # append new results
            summ <- rbind(summ,tempsumm)
          }
        }
        cells <- tinput[,eval(parse(text=express)),by=splitter]
        
        # labelling        
        set(cells,j=1:length(splitter),value=as.character(cells[[splitter]]))
        for(m in 1:length(splitter)){
          if(!(is.null(attributes(x[,splitter[m]])$value.labels))){
            for(l in 1:length(attributes(x[,splitter[m]])$value.labels)){    
              cells[which(cells[ , m , with = F] == as.integer(attributes(x[,splitter[m]])$value.labels[l])) , m] <- names(attributes(x[,splitter[m]])$value.labels[l])
            }
          }
        }
        
        result[[i]] <- list("summary" = summ,
                            "cells"=cells)
        names(result)[[i]] <- variables[[i]]
      }
    }
  } else {
    # count amount of vars
    for(i in 1:length(variables)){
      for(k in 1:length(by)){
        if(any(unique(attributes(x)$SPLIT_FILE) != F)){
          
          ### SPLIT FILE PART
          
          #####################################
          #
          # Summary
          #
          #
          #####################################
          
          # create empty data.frame for summary
          overall <-  data.frame()
          # create empty list 
          temp <- list()
          # get the group indicators
          pos <- unique(x[paste(unlist(str_split(splitter,pattern=",")))])
          # order indicators
          pos <- as.data.table(pos)
          setkeyv(x=pos,cols=colnames(pos))
          
          # indicate the group
          if(is.null(nrow(pos))){
            for(m in 1:length(pos)){  
              if(any(is.na(pos))){ #& k == 1)
                temp[[m]] <- x[which(is.na(x[,splitter])),]
              } else{
                temp[[m]] <- na.omit(x[eval(parse(text=paste(paste0("x$",splitter),"==",pos[m],collapse=" & "))),])
              }
              # get the row values
              rows <- eval(parse(text=paste0("temp[[m]]$",by[[k]])))
              # combine them
              overall <- rbind(overall,rows)
            }
          } else{
            for(m in 1:nrow(pos)){  
              if(any(is.na(pos))){
                naexpress <- paste(paste0("x$",names(pos)),"==",pos[m,],collapse=" & ")
                navec <- str_split_fixed(string=naexpress,pattern="&",n=length(pos))
                napos <- grep(pattern="NA",x=navec)
                navec[napos] <-  paste0("is.na(",str_sub(string=navec[napos],start=0,end=str_locate_all(pattern="==",navec[napos])[[1]][1]-1),")")
                temp[[m]] <- x[eval(parse(text=paste(navec, collapse=" & "))),]  
              }else{
                temp[[m]] <- x[eval(parse(text=paste(paste0("x$",names(pos)),"==",pos[m,],collapse=" & "))),]  
              }
              if(length(which(!is.na(str_locate(rownames(temp[[m]]),pattern="NA")[,1])))>=1){
                temp[[m]] <- temp[[m]][-which(!is.na(str_locate(rownames(temp[[m]]),pattern="NA")[,1])),]  
              } 
              overall <- rbind(overall,length(eval(parse(text=paste0("temp[[m]]$",by[[k]])))))
            }
          }
          ################################################
          # create a matrix with 6 colouns to prepare the output
          summ <- matrix(ncol=6)
          for(m in 1:nrow(overall)){
            # get missings
            if(any(is.na(eval(parse(text=paste0("temp[[m]]$",by[[k]])))))){
              leng <- length(which(is.na(eval(parse(text=paste0("temp[[m]]$",by[[k]]))))))
            } else{
              leng <- 0
            }          
            # get amount of obs
            overallleng <- overall[m,]
            # calculate the missing coloumns
            tempsumm <-  cbind("valid_obs."=overallleng- leng, 
                               "percent"=paste0(((overallleng - leng) /overallleng*100),"%"),
                               "missing obs."=leng,
                               "percent"=paste0((leng/overallleng*100),"%"),
                               "overall"=overallleng,
                               "percent"="100%")
            if(m==1){
              # create summ
              summ <- tempsumm
            } else{
              # append new results
              summ <- rbind(summ,tempsumm)
            }
          }
          #       for(k in 1:length(by)){
          ###############################
          ######################################################################
          # get indicators
          pos <- unique(x[c(unlist(str_split(string=paste(splitter,sep=","),pattern=",")))])
          # append inidcators left
          if(any(is.na(pos))){
            # reorder data, that NA is the first categorie
            pos <- as.data.table(pos)
            setkeyv(x=pos,cols=colnames(pos))
          } else{
            pos <- pos[order(pos[[1]]),]
          }  
          
          # create a matrix with the length of splitvars and the by var
          z <- as.matrix(cbind(pos,by[[k]]))
          
          summ <- cbind(z,summ)
          # label split vars
          for(l in 1:length(splitnames)){
            #if by is atomic
            varnam <- attributes(eval(parse(text=paste("x$",splitnames[[l]]))))$varname
            atts <- attributes(eval(parse(text=paste("x$",splitnames[[l]]))))$value.labels   
            if(is.null(nrow(pos))){
              colnames(summ)[which(colnames(summ) == "pos")]   <- varnam
            }
            temp <- summ[,which(colnames(summ) %in% varnam)] 
            
            if(any(is.na(temp))){
              temp[which(is.na(temp))] <- -9
              temp <- as.numeric(temp)
            }          
            for(m in 1:length(atts)){
              temp[which(temp==as.numeric(atts[m]))] <- names(atts[m])
            }
            summ[,which(colnames(summ) %in% varnam)] <- temp  
          }
          # label by var
          varnam <- attributes(eval(parse(text=paste("x$",by[[k]]))))$varname
          if(is.null(nrow(pos))){
            temp <- summ[,which(colnames(summ) %in% "")]
            temp <- attributes(eval(parse(text=paste("x$",by[[k]]))))$variable.label
            summ[,which(colnames(summ) %in% "")] <- temp
            colnames(summ)[which(colnames(summ) %in% "")] <- varnam
          } else{
            summ[,length(splitnames)+1]  <- attributes(eval(parse(text=paste("x$",by[[k]]))))$variable.label
          } 
        }else{
          #
          #
          # if splitfile is off
          #
          #
          #
          atts <- vector()
          atts <- c(atts,attributes(x[,by[k]])$variable.label)
          # calculate for every by the statistics
          overall <- cbind(x[,variables[i]],x[,by[k]])
          leng <- length(overall[is.na(overall)])
          overallleng <- length(overall[,1])
          # if j == 1 
          summ <-  cbind("valid_obs."=overallleng- leng, 
                         "percent"=paste0(((overallleng - leng) /overallleng*100),"%"),
                         "missing obs."=leng,
                         "percent"=paste0((leng/overallleng*100),"%"),
                         "overall"=overallleng,
                         "percent"="100%")  
          
          # cbind the by vars
          summ <- cbind(by,summ)
          summ[,which(colnames(summ) %in% "by")]  <- atts
          colnames(summ)[which(colnames(summ) %in% "by")] <- attributes(x[,variables[i]])$variable.label
        }
        
        ########################
        ##
        ## SPLIT FILE PART
        
        
        # check if split file is on
        if(unique(attributes(x)$SPLIT_FILE != FALSE)){
          # concate splitvars and by vars
          byarg <- paste(c(splitter,by[[k]]),collapse=",")  
          # calculate the cellstatistcs by the pasted express
          cells <- tinput[,eval(parse(text=express)),by=byarg]
          # sort ascending, set variable names
          setorderv(cells,cols=names(cells)[1:(length(splitnames)+1)])
          if(any(is.na(cells[,which(colnames(cells) %in% by[k]), with =F]))){
            cells <- cells[-which(is.na(cells[,which(colnames(cells) %in% by[k]), with =F])),]  
          }
          if(any(is.na(cells[, which(colnames(cells) %in% splitnames) ,with = FALSE]))){
            for(m in 1:(length(splitnames)+1)){
              if(any(is.na(cells[,m, with =F]))){
                cells[which(is.na(cells[,m, with =F])),m] <- -9
              }
            }
          }
          m <-  1
          vec <- vector()
          for(n in 1:(nrow(cells)-1)){    
            l <- n+1
            if(n == 1 & !(setequal(as.character(cells[n, which(colnames(cells) %in% splitnames) ,with = FALSE]), as.character(cells[l, which(colnames(cells) %in% splitnames),with=F])))){
              vec[m] <-n
              m <- m+1   
            }
            if(n > 1 & any(unique(cells[n, which(colnames(cells) %in% splitnames) ,with = FALSE] != cells[l, which(colnames(cells) %in% splitnames) ,with = FALSE]))){
              vec[m] <-n
              m <- m+1   
            }
          }
          
          out <- data.table()
          fillvec  <- vec+1
          temp <- tinput[,c(variables[i],splitter,by[k]),with=F]
          fill <- cbind("Overall",temp[,eval(parse(text=express)),by=splitter])
          # set names as character to able labeling
          set(fill,j=2:(1+length(splitter)),value=as.character(fill[[2:(1+length(splitter))]]))
          fill[,2:(1+length(splitter))] <- "_"
          for(m in 1:(length(vec)+1)){       
            if(m == 1){
              out <- rbind(out,cells[1:vec[m],])
            } else {
              if(m<=length(vec)){
                out <- rbind(out,cells[fillvec[m-1]:vec[m],])    
              } else{
                out <- rbind(out,cells[fillvec[m-1]:nrow(cells),])
              }   
            }          
            setnames(fill,old=names(fill),new=names(out))
            out <- rbind(out,fill[m])
          }
          
          # labelleing
          for(m in 1:length(unlist(str_split(string=byarg,pattern=",")))){
            if(!(is.null(attributes(out[[m]])$variable.label))){
              setnames(out,names(out)[[m]], attributes(out[[m]])$variable.label)
            } else{
              setnames(out,names(out)[[m]], attributes(out[[m]])$varname)
            }  
          }
          
          vars <- unlist(str_split(byarg,pattern=","))
          for(m in 1:length(vars)){
            if(!(is.null(attributes(x[,vars[m]])$value.labels))){
              for(l in 1:length(attributes(x[,vars[m]])$value.labels)){    
                out[which(out[ , m , with = F] == as.integer(attributes(x[,vars[m]])$value.labels[l])) , m] <- names(attributes(x[,vars[m]])$value.labels[l])
              }
            }
          }
        }else{
          # just take by vars
          byarg <- x[,by[k]]
          out <- tinput[,eval(parse(text=express)),by=byarg]
          if(any(is.na(out[1:nrow(out)]))){
            out <- out[-which(is.na(out[1:nrow(out)]))]
          }
          setkeyv(x=out,cols="byarg")
          setnames(out,names(out)[[1]], attributes(out[[1]])$varname)
          temp <- na.omit(tinput[, c(variables[i],by[k]), with =F])
          fill <- cbind("Overall",temp[,eval(parse(text=express)),])
          setnames(fill,old=names(fill),new=names(out))
          out <- rbind(out,fill)
        }
        
        if(unique(attributes(x)$SPLIT_FILE != FALSE)){
          vars <- unlist(str_split(byarg,pattern=","))
          for(m in 1:length(vars)){
            if(!(is.null(attributes(x[,vars[m]])$value.labels))){
              for(l in 1:length(attributes(x[,vars[m]])$value.labels)){    
                out[which(out[ , m , with = F] == as.integer(attributes(x[,vars[m]])$value.labels[l])) , m] <- names(attributes(x[,vars[m]])$value.labels[l])
              }
            }
          }
        } else{
          for(m in 1:length(by)){
            if(!(is.null(attributes(x[,by[m]])$value.labels))){
              for(l in 1:length(attributes(x[,by[m]])$value.labels)){    
                out[which(out[ , m , with = F] == as.integer(attributes(x[,by[m]])$value.labels[l])) , m] <- names(attributes(x[,by[m]])$value.labels[l])
              }
            }
          }
        }
        if(!(is.null(statistics))){
          if(is.element("anova",statistics)){
            anovas <- etas <- list()
            if(unique(attributes(x)$SPLIT_FILE != F)){
              group <- unique(tinput[,splitnames, with=F])
              setorder(group)
              for(m in 1:nrow(group)){
                subset <- x[eval(parse(text=paste(paste0("x$",splitnames),"==",group[m],collapse=" & "))),]
                anovadata <- eval(parse(text=paste("lm(subset[,variables[i]] ~ ", paste0("subset$",splitnames,collapse=" + ")," * subset[,by[k]])")))
                subset <- na.omit(subset)
                if(!(any(is.nan(anovadata[[3]]))) & (length(anovadata[[3]]) == 2)){
                  anovaout <- cbind(anovadata[[2]],anovadata[[1]],anovadata[[3]],anovadata[[4]],anovadata[[5]])
                  anovaout <- rbind(anovaout,c(sum(anovadata[[2]]),sum(anovadata[[1]]),"","",""))
                  colnames(anovaout) <- c("SumSquare","DF","MeanSquare","F","Significance")
                  
                  etasqrt <- sqrt(summary.lm(data)$r.squared)
                  etasquare <- summary.lm(data)$r.squared
                  anovas[[m]] <- anovaout
                  etas[[m]] <- cbind(etasqrt,etasquare)
                  if(!(is.null(attributes(x[,splitnames[n]])$value.labels))){
                    labels <- NULL
                    for(n in 1:length(splitnames)){
                      label <- names(attributes(eval(parse(text=paste0("x$",splitnames[n]))))$value.labels[which(as.numeric(attributes(eval(parse(text=paste0("x$",splitnames[n]))))$value.labels) %in% group[m,n,with=F])])
                      labels <- paste(label,labels, sep=" + ")
                    }
                    labels <- str_sub(string=labels,start=1,end=str_length(labels)-2)  
                    names(anovas)[[m]] <- labels
                    names(etas)[[m]] <- labels
                  } 
                }
              }
              if(length(anovas) !=0){
                for(n in length(anovas):1){
                  if(is.null(anovas[[n]])){
                    anovas[[n]] <- NULL
                    etas[[n]] <- NULL
                  }
                }  
              } else{
                anovas[[n]] <- NULL
                etas[[n]] <- NULL
              }              
              variance <- list("Anova" = anovas, "Eta" = etas)
              
            } else{
              data <- eval(parse(text=paste("lm(x[,variables[i]] ~ x[,by[k]])")))
              anovadata <- anova(data)
              anovaout <- cbind(anovadata[[2]],anovadata[[1]],anovadata[[3]],anovadata[[4]],anovadata[[5]])
              anovaout <- rbind(anovaout,c(sum(anovadata[[2]]),sum(anovadata[[1]]),"","",""))
              colnames(anovaout) <- c("SumSquare","DF","MeanSquare","F","Significance")
              etasqrt <- sqrt(summary.lm(data)$r.squared)
              etasquare <- summary.lm(data)$r.squared
              labels <- NULL
            }
            variance <- list("anova" = anovaout, "eta" = cbind(etasqrt,etasquare))
            
          }
        }
        summs[[k]] <- summ
        names(summs)[[k]] <- paste0("by",by[k])
        outs[[k]] <- out
        names(outs)[[k]] <- paste0("by",by[k])
        if(!(is.null(statistics))){
          variances[[k]] <- variance
          names(variances)[[k]] <- paste0("by",by[k])
        }        
      }
      if(is.null(statistics)){
        result[[i]] <- list("summary" = summs, "cells"=outs)  
      } else{
        if(length(by) == 1){
          result[[i]] <- list("summary" = summs[[1]], "cells"=outs[[1]], "anova"=variances[[1]]$anova, "eta"=variances[[1]]$eta)      
        } else{
          result[[i]] <- list("summary" = summs, "cells"=outs, "variance"=variances)      
        }
        #label output 
      }
      names(result)[i] <- paste0(variables[i])     
    }
  }
  
  options(warn=0)
  return(result)
}