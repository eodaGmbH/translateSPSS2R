#' Simple descriptive statistics
#'
#' R Implementation of the SPSS Function \code{FREQUENCIES}.
#'
#' @details The xpssFrenquencies function provides a set of descriptive statistic tools. The function delivers frequency tables containing value labels, values, frequencies, percentages of the selected variables in the dataset. Furthermore, xpssFrequency supplies three types of visualization of categorical or continous numerical:
#' \enumerate{
#' \item barchart
#' \item histogram
#' \item piechart
#' }
#' 
#' It is possible to customize the graphics by indiviual parameters. If TRUE is set, the default graphic will be plotted.
#'
#' \strong{\code{individual graphic parameter (for all charts):}}
#' 
#' \tabular{rll}{
#' \tab \code{max=n} \tab Cut the amount of maximum till n elements.
#' \cr \tab \code{min=n} \tab Cut the amount of n till minimum elements.
#' \cr \tab \code{freq=n} \tab Displays the distrubtion in absolute values on the basis of a user-defined maxima, the maxima has to be higher then the maxima of the distribtion, freq=max(n) is the default. (except piechart).
#' \cr \tab \code{percent=n} \tab Displays the distrubtion in relative values on the basis of a user-defined maxima, the maxima has to be higher then the maxima of the distribtion, percent=max(n) is the default. (except piechart).
#' }
#' \strong{\code{individual graphic parameter (for histogram):}}
#' \tabular{rll}{
#' \tab \code{normal=T} \tab Draws a overlapping normal curve.
#' }
#' \strong{\code{individual graphic parameter (for piechart):}}
#' \tabular{rll}{
#' \tab \code{missing=T} \tab Displays or excludes Missing Values.
#' }
#' 
#' \strong{\code{statistics:}}
#' \tabular{rll}{
#' 
#'\tab \code{kurtosis} \tab calculates the bulge of the variable. 
#'\cr \tab \code{maxixmum} \tab displays the maximum of the variable. 
#'\cr \tab \code{mean} \tab calculates the arithmetic mean.
#'\cr \tab \code{median} \tab calculates the median.
#'\cr \tab \code{minimum} \tab displays the minimum of the variable. 
#'\cr \tab \code{mode} \tab displays the modal value of the variable.
#'\cr \tab \code{none} \tab displays no statistics.
#'\cr \tab \code{range} \tab displays the span between the minimum and the maximum value. 
#'\cr \tab \code{sekurtosis} \tab calculates the standrard error of the bulge of the variable. 
#'\cr \tab \code{semean} \tab displays the standard error of the arithmetic mean. 
#'\cr \tab \code{seskewness} \tab calculates the standrard error of the inclination of the variable. 
#'\cr \tab \code{skewness} \tab calculates the inclination of the variable. 
#'\cr \tab \code{stddev} \tab  displays the standard deviation of the variable. 
#'\cr \tab \code{sum} \tab calculates the sum of each observation within the variable. 
#'\cr \tab \code{variance} \tab displays the variance.}
#'
#' @param x a (non-empty) data.frame or input data of class \code{"xpssFrame"}. 
#' @param variables atomic character or character vector with the name of the variables.
#' @param missing atomic character which specifiy the missing method. The method indicates what should happen when the data contains NAs. Default is \code{"NULL"}. Optionally it is possible to include user-defined missing values with \code{"include"}.
#' @param barchart plot a barchart. Default for \code{plot} is \code{NULL}. If \code{plot} is \code{TRUE} an default barchart will be plotted. Optional for customized barchart a list with the following arguments has to be assigned \code{minimum(n)}, \code{maximum(n)} to bound lower and upper values which are not plotted. See notes for more.
#' @param piechart plot a piechart. Default for \code{plot} is \code{NULL}. If \code{plot} is \code{TRUE} an default an default piechart will be plotted. Optional for customized piechart a list with the following arguments has to be assigned \code{minimum(n)}, \code{maximum(n)} to bound lower and upper values which are not plotted. See notes for more.
#' @param histogram plot a histogram.  Default for \code{plot} is \code{NULL}. If \code{plot} is \code{TRUE} an default histogram will be plotted. Optional for customized histogram a list with the following arguments has to be assigned \code{minimum(n)}, \code{maximum(n)} to bound lower and upper values which are not plotted. With \code{normal} a overlapping normal distrubtion line will drawn. Default is \code{FALSE}.  See notes for more.
#' @param ntiles divides the distribution in a specific percentage amount of categories. multiple dividing in distributions is allowed. Default is \code{NULL}.
#' @param percentiles displays the value between customized percentiles. Default is \code{NULL}.
#' @param statistics Method which enumerate the descriptive statistics. Default is \code{mean}, \code{stddev}, \code{minimum}, \code{maximum}. Optional arguments are \code{all},\code{kurtosis}, \code{median}, \code{mode}, \code{none}, \code{range}, \code{sekurt}, \code{semean}, \code{seskew}, \code{skewness}, \code{sum}, \code{variance}.
#' @importFrom e1071 kurtosis skewness
#' @importFrom graphics pie hist barplot
#' @importFrom data.table as.data.table set
#' @author Bastian Wiessner
#' @examples
#' data(fromXPSS)
#' xpssFrequencies(x=fromXPSS,  
#'  variables=c("V5"))
#'  
#' xpssFrequencies(x=fromXPSS,
#'  variables=c("V3","V7_2"),
#'  ntiles=c(0.25,0.3),
#'  percentiles=c(0.23,0.46,0.88))
#'  
#' xpssFrequencies(x=fromXPSS,
#'  variables=c("V3","V7_2"),
#'  histogram=list(plot=TRUE))
#'  
#'  xpssFrequencies(x=fromXPSS,
#'  variables=c("V3"),
#'  piechart=list(plot=TRUE,min=0,max=2))
#'  
#'  xpssFrequencies(x=fromXPSS,
#'  variables=c("V3"),
#'  barchart=list(plot=TRUE,precent=50))
#'  
#' @export
#' 

xpssFrequencies  <- function(x,
                             variables,
                             missing = NULL,
                             barchart = list(plot=FALSE,
                                             min=NULL,
                                             max=NULL,
                                             freq=NULL,
                                             percent=NULL),
                             piechart = list(plot=FALSE,
                                             min=NULL,
                                             max=NULL,
                                             freq = NULL,
                                             percent = NULL,
                                             missing=FALSE),
                             histogram = list(plot=FALSE,
                                              min=NULL,
                                              max=NULL,
                                              freq=NULL,
                                              percent=NULL,
                                              normal=FALSE),
                             ntiles = NULL,
                             percentiles = NULL,
                             statistics = c("mean",
                                            "stddev",
                                            "minimum",
                                            "maximum")){
  
  #set ask True for more than 1 plot, return Enter to go to the next plot
  #par(ask=T)
  
  
  # define function type
  functiontype <- "AN"
  # do meta check
  x <- applyMetaCheck(x)
  
  options(warn=-1)
  
  .N <- NULL
  
  # exception check if the named variables are in the dataset
  for(i in 1:length(variables)){
    if(!(is.element(variables[i],names(x)))) {
      stop("The selected variables are not in the dataset")
    }
  }
  
  for(i in 1:length(variables))  {
    if(class(x[,variables[i]]) != "numeric"){  
      stop("Variables are not numeric")
    }
  } 
  ## if missing is include 
  
  # include missing values
  if("include" %in% missing)
  {
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
  }
  
  
  ### data.table object
  if(unique(attributes(x)$SPLIT_FILE != FALSE)){
    # split the variablenames
    splitter <- unlist(str_split(attributes(x)$SPLIT_FILE,pattern=" "))
    # exclude the the last statement
    splitter <- splitter[1:length(splitter)-1]
    splitnames <- unlist(str_split(splitter,pattern=","))
    # combine variables and split vars
    vars <- c(variables,splitnames)
    ### create subset
    pos <- which(names(x)%in%vars)  
    # create data.table object
    tinput <- data.table(x[pos])  
    # combine the split vars for data.table operations
    splitter  <- byarg <- paste(splitter,collapse=",")
  }
  # placeholder for output
  frequencies <- list()
  #global loop
  for(i in 1:length(variables)) {
    # check if split file is on
    if(any(attributes(x)$SPLIT_FILE != F)){
      # create the split argument
      splitter <- paste(byarg,variables[i],sep=",")
      # calculate frequencies
      freqWiNa <- tinput[, list(frequency=.N) , by = splitter]
      #######################################
      # reorder the data, na is last
      setorderv(freqWiNa,cols=c(splitnames,variables[i]),na.last=T)
      
      # get the group indizes
      pos <- unique(tinput[,splitnames,with=F])      
      # create placeholder
      sums <- freqWiNa[,sum(frequency),by=splitnames]
      for(m in 1:nrow(pos)){
        sums[m] <- length(na.omit(eval(parse(text=paste0("x[eval(parse(text=paste(paste0('x$',splitnames),'==',pos[m],collapse=' & '))),]$",variables[i])))))  
      }
      
      #manual counter for group statistics
      k <- 1
      # placeholder for the output
      out <- NULL
      # loop over all split variables
      for(l in 1:(nrow(freqWiNa[,1:length(splitnames),with=F]))){
        # if indicator is equal with the last row of the data
        if(l == nrow(freqWiNa[,1:length(splitnames),with=F])){
          # check if there is any na
          if(any(is.na(freqWiNa[l,variables[i],with=F]))){
            # if there is na, calculate group statistics without na
            fill <- c("Overall","in","group",rep("_",times=length(out)-4),sums$V1[k])
            # rbind data
            out <- rbind(out,as.list(fill))
            # rbind the na data
            out <- rbind(out,cbind("Missing",freqWiNa[l,]))
            # calculate group statistics with na
            fill <- c("Overall","with","missing",rep("_",times=length(out)-4),sums$V1[k]+freqWiNa[l,frequency])
            # rbind result
            out <- rbind(out,as.list(fill))
            # if last row doesnt have any na
          } else{
            # create placeholder
            fill <- c("Overall",rep("_",times=length(out)-2),sums$V1[k])
            # rbind data
            out <- rbind(out,cbind("_",freqWiNa[l,]))
            # add statistics
            out <- rbind(out,as.list(fill))
          } 
        } else{
          # check for grouping. if the splitvar values are identical they are a group, dont calculate statistics,
          # else calculate statitics after the row got rbinded
          if(setequal(as.character(freqWiNa[l,1:length(splitnames),with=F]), as.character(freqWiNa[l+1,1:length(splitnames),with=F]))){
            # if its the first row and part of a group
            if(l == 1){
              # add a _ to the data
              out <- cbind("_",freqWiNa[l,])
            } else{
              # if its another row then 1, rbind it with _
              out <- rbind(out,cbind("_",freqWiNa[l,]))
            } 
          } else{
            # if its the last element of a group
            if(l == 1){
              # add a overall statement
              out <- cbind("Overall",freqWiNa[l,] )
              # count manually +1
              k <- k+1
            } else{
              #if the data contains NA
              if(any(is.na(freqWiNa[l,variables[i],with=F]))){
                # create output structure
                fill <- c("Overall","in","group",rep("_",times=length(out)-4),sums$V1[k])
                # rbind the overall data without NA
                out <- rbind(out,as.list(fill))
                # rbind the missing data
                out <- rbind(out,cbind("Missing",freqWiNa[l,]))
                # calc the group statistics with na
                fill <- c("Overall","with","missing",rep("_",times=length(out)-4),sums$V1[k]+freqWiNa[l,frequency])
                # rbind this
                out <- rbind(out,as.list(fill))
                # count manually +1
                k <- k+1
              } else{
                # create output structure
                fill <- c("Overall",rep("_",times=length(out)-2),sums$V1[k])
                # rbind actual row
                out <- rbind(out,cbind("_",freqWiNa[l,]))
                # rbind group statistics
                out <- rbind(out,as.list(fill))
                # count manually +1
                k <- k+1
              }              
            }            
          }
        }
      }
      # indicator for the groups
      pos <- which(out[,1,with=F] == "Overall")
      # create perc placeholder
      perc <- NULL
      # count from 1 to amount of elements in pow
      percspace <- length(splitnames)+3
      for(m in 1:length(pos)){
        # if the first element in pos is equal to 1
        if(1 == pos[m]){
          # set 100% as value
          perc <- 1.00
        } else{
          if(m == 1){
            if(out[pos[m]-1,1,with=F]== "Missing"){
              for(n in (1 : (pos[m]))){
                perc <- rbind(perc,as.numeric(out[n,percspace,with=F]) / as.numeric(out[pos[m],percspace,with=F]))
              }
            }
            if(!(out[pos[m]-1,1,with=F]== "Missing" | out[pos[m]+1,1,with=F]== "Missing")){
              for(n in (1 : (pos[m]))){
                perc <- rbind(perc,as.numeric(out[n,percspace,with=F]) / as.numeric(out[pos[m],percspace,with=F]))
              }
            }
          } else{
            if(pos[m] != max(pos)){
              # if the next one is a missing
              if(out[pos[m]+1,1,with=F]== "Missing"){
                for(n in (pos[m-1]+1) : (pos[m])){
                  perc <- rbind(perc,as.numeric(out[n,percspace,with=F]) / as.numeric(out[pos[m]+2,percspace,with=F]))
                }
              }
              if(out[pos[m]-1,1,with=F]== "Missing"){
                for(n in (pos[m-1]+1) : (pos[m])){
                  perc <- rbind(perc,as.numeric(out[n,percspace,with=F]) / as.numeric(out[pos[m],percspace,with=F]))
                }
              }
              if(!(out[pos[m]-1,1,with=F]== "Missing" | out[pos[m]+1,1,with=F]== "Missing")){
                for(n in (pos[m-1]+1) : (pos[m])){
                  perc <- rbind(perc,as.numeric(out[n,percspace,with=F]) / as.numeric(out[pos[m],percspace,with=F]))
                }
              }  
            } else{
              if(out[pos[m]-1,1,with=F]== "Missing"){
                for(n in (pos[m-1]+1) : (pos[m])){
                  perc <- rbind(perc,as.numeric(out[n,percspace,with=F]) / as.numeric(out[pos[m],percspace,with=F]))
                }
              }
              if(!(out[pos[m]-1,1,with=F]== "Missing" | !(is.na(out[pos[m]+1,1,with=F]== "Missing")))){
                for(n in (pos[m-1]+1) : (pos[m])){
                  perc <- rbind(perc,as.numeric(out[n,percspace,with=F]) / as.numeric(out[pos[m],percspace,with=F]))
                }
              }
            }
          }
        }
      }
      perc <- perc*100
      out <- cbind(out,"Percent"=paste0(perc,"%"))
      
      pos <- which(out[,1,with=F] == "Overall")
      perc <- NULL
      for(m in 1:length(pos)){
        if(1 == pos[m]){
          perc <- 1.00
        } else{
          if(m == 1){
            if(out[pos[m]+1,1,with=F]== "Missing"){
              for(n in (1 : (pos[m]))){
                
                perc <- rbind(perc,round(as.numeric(out[n,percspace,with=F]) / as.numeric(out[pos[m],percspace,with=F]),digits=3))
              }
              perc <- rbind(perc,"_")
              perc <- rbind(perc,"_")
            }
            if(!(out[pos[m]-1,1,with=F]== "Missing" | out[pos[m]+1,1,with=F]== "Missing")){
              for(n in (1 : (pos[m]))){
                perc <- rbind(perc,round(as.numeric(out[n,percspace,with=F]) / as.numeric(out[pos[m],percspace,with=F]),digits=3))
              }
            }
          } else{
            if(pos[m] != max(pos)){
              # if the next one is a missing
              if(out[pos[m]+1,1,with=F]== "Missing"){
                for(n in (pos[m-1]+1) : (pos[m])){
                  perc <- rbind(perc,round(as.numeric(out[n,percspace,with=F]) / as.numeric(out[pos[m],percspace,with=F]),digits=3))
                }
                perc <- rbind(perc,"_")
                perc <- rbind(perc,"_")
              }
              if(!(out[pos[m]-1,1,with=F]== "Missing" | out[pos[m]+1,1,with=F]== "Missing")){
                for(n in (pos[m-1]+1) : (pos[m])){
                  perc <- rbind(perc,round(as.numeric(out[n,percspace,with=F]) / as.numeric(out[pos[m],percspace,with=F]),digits=3))
                }
              }  
            } else{
              if(!(out[pos[m]-1,1,with=F]== "Missing" | !(is.na(out[pos[m]+1,1,with=F]== "Missing")))){
                for(n in (pos[m-1]+1) : (pos[m])){
                  perc <- rbind(perc,as.numeric(out[n,percspace,with=F]) / as.numeric(out[pos[m],percspace,with=F]))
                }
              }
            }
          }
        }
      }
      
      perc <- as.numeric(perc)*100
      pos <- which(is.na(perc))
      out <- cbind(out,"Valid Percent"=paste0(perc,"%"))
      out[pos,ncol(out)] <- "_"
      
      pos <- which(out[,1,with=F] == "Overall")
      perc <- NULL
      for(m in 1:length(pos)){
        if(1 == pos[m]){
          perc <- 1.00
        } else{
          if(m == 1){
            if(out[pos[m]+1,1,with=F]== "Missing"){
              for(n in (1 : (pos[m]-1))){
                if(n == 1){
                  perc <- rbind(perc,round(as.numeric(out[n,percspace,with=F]) / as.numeric(out[pos[m],percspace,with=F]),digits=3))  
                } else{
                  if(n <= pos[m]-1){
                    perc <- rbind(perc,as.numeric(perc[n-1]) + round(as.numeric(out[n,percspace,with=F]) / as.numeric(out[pos[m],percspace,with=F]),digits=3))  
                  }                 
                }                
              }
              perc <- rbind(perc,"_")
              perc <- rbind(perc,"_")
              perc <- rbind(perc,"_")
            }
            if(!(out[pos[m]-1,1,with=F]== "Missing" | out[pos[m]+1,1,with=F]== "Missing")){
              for(n in (1 : (pos[m]-1))){
                if(n == 1){
                  perc <- rbind(perc,round(as.numeric(out[n,percspace,with=F]) / as.numeric(out[pos[m],percspace,with=F]),digits=3))  
                } else{
                  if(n <= pos[m]-1){
                    perc <- rbind(perc,as.numeric(perc[n-1]) + round(as.numeric(out[n,percspace,with=F]) / as.numeric(out[pos[m],percspace,with=F]),digits=3))  
                  }                 
                }
              }  
              perc <- rbind(perc,"_")
            }
          } else {
            if(pos[m] != max(pos)){
              # if the next one is a missing
              if(out[pos[m]+1,1,with=F]== "Missing"){
                for(n in (pos[m-1]+1) : (pos[m]-1)){
                  if(n == pos[m-1]+1){
                    perc <- rbind(perc,round(as.numeric(out[n,percspace,with=F]) / as.numeric(out[pos[m],percspace,with=F]),digits=3))  
                  } else{
                    if(n <= pos[m]-1){
                      perc <- rbind(perc,as.numeric(perc[n-1]) + round(as.numeric(out[n,percspace,with=F]) / as.numeric(out[pos[m],percspace,with=F]),digits=3))  
                    }                 
                  }                
                }
                perc <- rbind(perc,"_")
                perc <- rbind(perc,"_")
                perc <- rbind(perc,"_")
              }
              if(!(out[pos[m]-1,1,with=F]== "Missing" | out[pos[m]+1,1,with=F]== "Missing")){
                for(n in (pos[m-1]+1) : (pos[m]-1)){
                  if(n == pos[m-1]+1){
                    perc <- rbind(perc,round(as.numeric(out[n,percspace,with=F]) / as.numeric(out[pos[m],percspace,with=F]),digits=3))  
                  } else{
                    if(n <= pos[m]-1){
                      perc <- rbind(perc,as.numeric(perc[n-1]) + round(as.numeric(out[n,percspace,with=F]) / as.numeric(out[pos[m],percspace,with=F]),digits=3))  
                    }                 
                  }
                }  
                perc <- rbind(perc,"_")
              }
            }else{
              if(!(out[pos[m]-1,1,with=F]== "Missing" | !(is.na(out[pos[m]+1,1,with=F]== "Missing")))){
                for(n in (pos[m-1]+1) : (pos[m]-1)){
                  if(n == pos[m-1]+1){
                    perc <- rbind(perc,round(as.numeric(out[n,percspace,with=F]) / as.numeric(out[pos[m],percspace,with=F]),digits=3))  
                  } else{
                    if(n <= pos[m]-1){
                      perc <- rbind(perc,as.numeric(perc[n-1]) + round(as.numeric(out[n,percspace,with=F]) / as.numeric(out[pos[m],percspace,with=F]),digits=3))  
                    }                 
                  }
                }  
                perc <- rbind(perc,"_")
              }
            }
          }
        }
      }
      perc <- as.numeric(perc)*100
      pos <- which(is.na(perc))
      out <- cbind(out,"Cum. Percent"=paste0(perc,"%"))
      out[pos,ncol(out)] <- "_"
      
      ######## Labelling
      vars <- c(variables[i],splitnames)
      for(m in 1:length(vars)){
        val <- sort(as.numeric(eval(parse(text=paste0("attributes(x$",vars[m],")$value.labels")))))
        if(length(val)>0){
          lab <- names(sort(eval(parse(text=paste0("attributes(x$",vars[m],")$value.labels")))))
          for(n in 1:length(val)){
            out[which(out[,which(names(out) == vars[m]),with=F] == val[n]),which(names(out) == vars[m])] <- lab[n]
          }  
        }        
      }
      
      freq <- out
      
      
      ########################################################################################################################################
      
      
      
      if(is.null(statistics)){
        pos <- x[paste(unlist(str_split(splitter,pattern=",")))]
        for(m in 1:length(pos)){
          pos[[m]] <- as.character(pos[[m]])
        }      
        # order indicators
        pos <- as.data.table(unique(pos[-c(length(pos))]))
        setkeyv(x=pos,cols=colnames(pos))
        for(m in 1:nrow(pos)){
          nmiss <- nrow(x[eval(parse(text=paste(paste0("x$",splitnames),"==",pos[m,],collapse=" & "))),]) - sums[m]$V1
          if(m ==1){
            perc <- cbind("valid" = sums[m]$V1, "miss" = nmiss)  
          } else{
            temp <- cbind("valid" = sums[m]$V1, "miss" = nmiss)  
            perc <- rbind(perc,temp)
          }
        }
        for(m in 1:length(pos)){
          pos[,m] <- as.character(pos[,m])
          if(!(is.null(attributes(x[,names(pos)[m]])$value.labels))){
            val <- sort(as.numeric(eval(parse(text=paste0("attributes(x$",names(pos)[m],")value.labels")))))
            lab <- names(sort(eval(parse(text=paste0("attributes(x$",names(pos)[m],")value.labels")))))
            for(n in 1:length(val)){
              pos[which(pos[,m,with=F] == val[n]),m] <- lab[n]
            }  
          }
        }
        descr <- cbind(pos,perc)
      } else{
        express <- "list("
        if(is.element("default",statistics)){
          express <- paste0(express,"mean=mean(get(variables[[i]]),na.rm=T),sd=sd(get(variables[[i]]),na.rm=T),max=max(get(variables[[i]]),na.rm=T),min=min(get(variables[[i]]),na.rm=T),")
        }
        if(is.element("kurtosis",statistics)){
          express <- paste0(express,"kurt=if(length(na.omit(get(variables[[i]])))>=4) kurtosis(get(variables[[i]]),na.rm=T,type=2) else 0,")
        }
        if(is.element("maximum",statistics)){
          express <- paste0(express,"max=max(get(variables[[i]]),na.rm=T),")
        }
        if(is.element("mean",statistics)){
          express <- paste0(express,"mean=mean(get(variables[[i]]),na.rm=T),")
        }
        if(is.element("median",statistics)){
          express <- paste0(express,"median=median(get(variables[[i]]),na.rm=T),")
        }
        if(is.element("minimum",statistics)){
          express <- paste0(express,"min=min(get(variables[[i]]),na.rm=T),")
        }
        if(is.element("mode",statistics)){
          express <- paste0(express,"mode=names(table(x$V6_kl3)[which(table(x$V6_kl3) %in% max(table(x$V6_kl3)))]),")
        }
        if(is.element("range",statistics)){
          express <- paste0(express,"range=diff(range(get(variables[[i]]),na.rm=T)),")
        }
        if(is.element("sekurt",statistics)){
          express <- paste0(express,"sekurt=2*(sqrt((6*length(na.omit(get(variables[[i]])))*(length(na.omit(get(variables[[i]])))-1)) / ((length(na.omit(get(variables[[i]])))-2)*(length(na.omit(get(variables[[i]])))+1)*(length(na.omit(get(variables[[i]])))+3))))* (sqrt(((length(na.omit(get(variables[[i]])))^2 -1)) / ((length(na.omit(get(variables[[i]])))-3)*(length(na.omit(get(variables[[i]])))+5)))),")
        }
        if(is.element("semean",statistics)){
          express <- paste0(express,"semean=sd(get(variables[[i]]),na.rm=T)/(sqrt(length(na.omit(get(variables[[i]]))))),")
        }
        if(is.element("skewness",statistics)){
          express <- paste0(express,"skewness=if(length(na.omit(get(variables[[i]])))>=3) skewness(get(variables[[i]]),na.rm=T,type=2) else 0,")
        }
        if(is.element("seskew",statistics)){
          express <- paste0(express,"seskew=sqrt((6*length(na.omit(get(variables[[i]])))*(length(na.omit(get(variables[[i]])))-1)) / ((length(na.omit(get(variables[[i]])))-2)*(length(na.omit(get(variables[[i]])))+1)*(length(na.omit(get(variables[[i]])))+3))),")
        }
        if(is.element("stddev",statistics)){
          express <- paste0(express,"stddev=sd(get(variables[[i]]),na.rm=T),")
        }
        if(is.element("sum",statistics)){
          express <- paste0(express,"sum=sum(get(variables[[i]]),na.rm=T),")
        }
        if(is.element("variance",statistics)){
          express <- paste0(express,"variance=var(get(variables[[i]]),na.rm=T),")
        }
        if(is.element("all",statistics)){
          express <- "list(kurt=if(length(na.omit(get(variables[[i]])))>=4) kurtosis(get(variables[[i]]),na.rm=T,type=2) else 0,max=max(get(variables[[i]]),na.rm=T),mean=mean(get(variables[[i]]),na.rm=T),median=median(get(variables[[i]]),na.rm=T),min=min(get(variables[[i]]),na.rm=T),mode=names(table(x$V6_kl3)[which(table(x$V6_kl3) %in% max(table(x$V6_kl3)))]),range=diff(range(get(variables[[i]]),na.rm=T)),sd=sd(get(variables[[i]]),na.rm=T),sekurt=2*(sqrt((6*length(na.omit(get(variables[[i]])))*(length(na.omit(get(variables[[i]])))-1)) / ((length(na.omit(get(variables[[i]])))-2)*(length(na.omit(get(variables[[i]])))+1)*(length(na.omit(get(variables[[i]])))+3))))* (sqrt(((length(na.omit(get(variables[[i]])))^2 -1)) / ((length(na.omit(get(variables[[i]])))-3)*(length(na.omit(get(variables[[i]])))+5)))),semean=sd(get(variables[[i]]),na.rm=T)/(sqrt(length(na.omit(get(variables[[i]]))))),skewness=if(length(na.omit(get(variables[[i]])))>=3) skewness(get(variables[[i]]),na.rm=T,type=2) else 0,seskew=sqrt((6*length(na.omit(get(variables[[i]])))*(length(na.omit(get(variables[[i]])))-1)) / ((length(na.omit(get(variables[[i]])))-2)*(length(na.omit(get(variables[[i]])))+1)*(length(na.omit(get(variables[[i]])))+3))),stddev=sd(get(variables[[i]]),na.rm=T),sum=sum(get(variables[[i]]),na.rm=T),variance=var(get(variables[[i]]),na.rm=T),"
          
        }
        express <- str_sub(string=express,start=1,end=str_length(express)-1)  
        # eval the pasted expression  
        express <- paste0(express,")")
        
        descr <- tinput[,eval(parse(text=express)),by=splitnames]
        for(k in 1:length(splitnames)){
          if(!(is.null(attributes(x[,splitnames[k]])$value.labels))){
            set(x=descr,j=k,value=as.character(descr[[splitnames[k]]]))
            for(i in 1:length(attributes(x[,splitnames[k]])$value.labels)){
              descr[i,k] <- names(attributes(x[,splitnames[k]])$value.labels[which(as.integer(attributes(x[,splitnames[k]])$value.labels) %in% descr[i,k,with=F])])
            }
          }  
        }
        
        
      }
      
      ######################################################
      
      
      # calculate ntiles if the paramester is set
      if(is.null(ntiles) == F) {
        ntile <- vector()
        out <- list()
        for(j in 1:length(ntiles)){
          # catch wrong input
          ntile[[j]] <- 1/ntiles[[j]]   
        } 
        pos <- x[splitnames]
        for(m in 1:length(pos)){
          pos[[m]] <- as.character(pos[[m]])
        }      
        pos <- as.data.table(unique(pos))
        setkeyv(x=pos,cols=colnames(pos))
        for(m in 1:nrow(pos)){
          data <- x[eval(parse(text=paste(paste0("x$",splitnames),"==",pos[m,],collapse=" & "))),]  
          if(length(ntile)>1){
            for(j in 1:length(ntile)){
              if(j == 1){
                temp <-quantile(data[,variables[i]],probs= seq(ntile[[j]],1-ntile[[j]], ntile[[j]]),na.rm=T)  
              } else{
                temp <- c(temp,quantile(data[,variables[i]],probs= seq(ntile[[j]],1-ntile[[j]], ntile[[j]]),na.rm=T))
              }
            }        
          } else{
            temp <-quantile(data[,variables[i]],probs= seq(ntile[[1]],1-ntile[[1]], ntile[[1]]),na.rm=T)  
          }
          ordervec <- gsub(pattern="%",replacement="",x=names(temp))
          names(temp) <- ordervec
          out[[m]] <- temp[order(as.numeric(names(temp)))]
          names(out[[m]]) <- paste0(names(out[[m]]),"%")
        }
        ntile <- out
        ######## Labelling
        pos <- unique(x[splitnames])
        for(m in 1:nrow(pos)){
          lab <- paste0("ntile",m)
          for(n in 1:length(pos)){
            if(length(eval(parse(text=paste0("attributes(x$",splitnames[n],")$value.labels"))))>0){
              if(n==1)  {
                lab <- names(which(eval(parse(text=paste0("attributes(x$",splitnames[n],")$value.labels"))) == pos[m,n]))
              } else{
                temp <- names(which(eval(parse(text=paste0("attributes(x$",splitnames[n],")$value.labels"))) == pos[m,n]))
                lab <- paste0(lab,"_",temp)
              }  
            }
          }
          names(ntile)[m] <- lab
        }
      } else{
        ntile <- NULL
      }
      
      # calculate the percentiles if the parameter is set
      if(is.null(percentiles) == F) {
        percentile <- NULL  
        out <- list()
        pos <- x[splitnames]
        percentile <- percentiles / 100
        for(m in 1:length(pos)){
          pos[[m]] <- as.character(pos[[m]])
        }      
        # order indicators
        pos <- as.data.table(unique(pos))
        setkeyv(x=pos,cols=colnames(pos))
        for(m in 1:nrow(pos)){
          data <- x[eval(parse(text=paste(paste0("x$",splitnames),"==",pos[m,],collapse=" & "))),]  
          
          out[[m]] <-  quantile(data[,variables[i]],probs= percentile,na.rm=T)
          
        }
        percentile <- out
        
        ######## Labelling
        pos <- unique(x[splitnames])
        for(m in 1:nrow(pos)){
          lab <- paste0("percentile",m)
          for(n in 1:length(pos)){
            if(length(eval(parse(text=paste0("attributes(x$",splitnames[n],")$value.labels"))))>0){
              if(n==1)  {
                lab <- names(which(eval(parse(text=paste0("attributes(x$",splitnames[n],")$value.labels"))) == pos[m,n]))
              } else{
                temp <- names(which(eval(parse(text=paste0("attributes(x$",splitnames[n],")$value.labels"))) == pos[m,n]))
                lab <- paste0(lab,"_",temp)
              }
            }
          }
          names(percentile)[m] <- lab
        }
      } else{
        percentile <- NULL
      }
      
      frequencies[[i]] <- list("freqs" = freq,
                               "stats" = descr,
                               "ntiles" = ntile,
                               "percintile" = percentile)
      
      if(is.null(frequencies[[i]]$percintile)){
        frequencies[[i]]$percintile <- NULL
      }
      if(is.null(frequencies[[i]]$ntiles)){
        frequencies[[i]]$ntiles <- NULL
      }
      if("none" %in% statistics){
        frequencies[[i]]$stats <- NULL
      }
      
      ########################################
      
      # plot histogram if T
      if(histogram$plot){
        if(is.list(histogram)) {
          #calc parameters for curve
          pos <- x[splitnames]
          for(m in 1:length(pos)){
            pos[[m]] <- as.character(pos[[m]])
          }      
          # order indicators
          pos <- as.data.table(unique(pos))
          setkeyv(x=pos,cols=colnames(pos))
          posgraph <- as.data.frame(pos)
          for(l in 1:nrow(pos)){
            temp <- x[eval(parse(text=paste(paste0("x$",splitnames),"==",pos[l,],collapse=" & "))),] 
            data = temp[,variables[i]]
            m<-mean(data,na.rm = T)
            std<-sqrt(var(data,na.rm = T))
            if(!(is.null(histogram$min)) && is.null(histogram$max)){         
              data <- ifelse(data>=histogram$min,data,NA)
            } 
            if((is.null(histogram$min)) && (!is.null(histogram$max))){       
              data <- ifelse(data<=histogram$max,data,NA)
            } 
            if(!(is.null(histogram$max)) && (!(is.null(histogram$min)))){         
              data <- ifelse(data<=histogram$max,data,NA)
              data <- ifelse(data>=histogram$min,data,NA)
            }
            if(!(is.null(histogram$freq))){
              if(histogram$freq < max(data)){
                stop("freq has to be equal or higher than the maximum value of the data")
              }
              if(!(is.null(histogram$normal))){
                if(histogram$normal){
                  h <- hist(data,plot=F)
                  hist(data,main="",ylab="",xlab="",ylim=c(0,histogram$freq),col=1:length(unique(data)))
                  multiplier <- h$counts / h$density
                  mydensity <- density(data,na.rm=T)
                  mydensity$y <- mydensity$y * multiplier[1]
                  lines(mydensity)
                }else{
                  hist(data,main="",ylab="",xlab="",col=1:length(unique(data)),ylim=c(0,histogram$freq),col=1:length(unique(data)))    
                }
              } 
            } 
            
            if(!(is.null(histogram$normal))){
              if(histogram$normal){
                h <- hist(data,plot=F)
                hist(data,main="",ylab="",xlab="",ylim=c(0,max(h$counts*1.5)),col=1:length(unique(data)))
                multiplier <- h$counts / h$density
                mydensity <- density(data,na.rm=T)
                mydensity$y <- mydensity$y * multiplier[1]
                lines(mydensity)
              }else{
                hist(data,main="",ylab="",xlab="",col=1:length(unique(data)))
              }
            }
            if(is.null(histogram$normal) & is.null(histogram$freq)){
              hist(data,main="",ylab="",xlab="")
            }
            
            
            ################
            # labelling
            label <- NULL
            for(k in 1:length(splitnames)){
              lab <- names(which(attributes(eval(parse(text=paste0("x$",splitnames[k]))))$value.labels == as.numeric(posgraph[l,k])))
              if(length(lab)>0){
                vallab <- attributes(eval(parse(text=paste0("x$",splitnames[k]))))$variable.label
                if(k == 1){              
                  label <- paste0(vallab,": ",lab)
                } else{
                  label <- paste0(label,", ",vallab,": ",lab)
                }
              }
            }
            if(is.null(attributes(x[,variables[i]])$variable.label)){
              title(main="",ylab="Frequency")
            } else{
              title(main=attributes(x[,variables[i]])$variable.label,ylab="Frequency",xlab=label)
            }
          }
        } 
      }
      # plot barchart if T
      if(barchart$plot){
        if(is.list(barchart)) {
          par(las=2) # make label text perpendicular to axis
          par(mar=c(5,8,4,2)) # increase y-axis margin.
          pos <- x[paste(unlist(str_split(splitter,pattern=",")))]
          for(m in 1:length(pos)){
            pos[[m]] <- as.character(pos[[m]])
          }      
          # order indicators
          pos <- as.data.table(unique(pos[-c(length(pos))]))
          setkeyv(x=pos,cols=colnames(pos))
          for(m in 1:nrow(pos)){
            temp <- x[eval(parse(text=paste(paste0("x$",splitnames),"==",pos[m,],collapse=" & "))),] 
            data = temp[,variables[i]]
            if(!(is.null(barchart$min)) && is.null(barchart$max)){        
              data <- ifelse(data>=barchart$min,data,NA)
            } 
            if((is.null(barchart$min)) && (!is.null(barchart$max))){          
              data <- ifelse(data<=barchart$max,data,NA)
            } 
            if(!(is.null(barchart$max)) && (!(is.null(barchart$min)))){         
              data <- ifelse(data<=barchart$max,data,NA)
              data <- ifelse(data>=barchart$min,data,NA)
            }
            data <- table(data)
            if(!(is.null(barchart$percent)) & is.null(barchart$freq)){
              if(barchart$percent != F){
                data <- data/sum(data)*100
                if(barchart$percent < max(data)){
                  stop("percent has to be equal or higher than the maximum value of the data")
                }
                if(!(is.null(attributes(x[,variables[i]])$value.labels))){
                  barplot(data,main=attributes(x[,variables[i]])$variable.label,ylim=c(0,barchart$percent),names.arg = names(attributes(x[,variables[i]])$value.labels),col=1:length(attributes(x[,variables[i]])$value.labels),xlab="Percent",horiz=T)    
                } else{
                  barplot(data,main=attributes(x[,variables[i]])$variable.label,ylim=c(0,barchart$percent),xlab="Percent",horiz=T)    
                }            
              }
            }
            if(is.null(barchart$percent) & (!(is.null(barchart$freq)))){
              if(barchart$freq < max(data)){
                stop("freq has to be equal or higher than the maximum value of the data")
              }
              if(!(is.null(attributes(x[,variables[i]])$value.labels))){
                barplot(data,main=attributes(x[,variables[i]])$variable.label,ylim=c(0,barchart$freq),names.arg = names(attributes(x[,variables[i]])$value.labels),col=1:length(attributes(x[,variables[i]])$value.labels),xlab="Absolute",horiz=T)    
              } else {
                barplot(data,main=attributes(x[,variables[i]])$variable.label,ylim=c(0,barchart$freq),xlab="Absolute",horiz=T)    
              }
            }
            if(is.null(barchart$percent) & is.null(barchart$freq)){
              if(!(is.null(attributes(x[,variables[i]])$value.labels))){
                barplot(data,main=attributes(x[,variables[i]])$variable.label,names.arg = names(attributes(x[,variables[i]])$value.labels)[as.numeric(names(data))],col=1:length(attributes(x[,variables[i]])$value.labels),xlab="Absolute",horiz=T)    
                
              } else {
                barplot(data,main=attributes(x[,variables[i]])$variable.label,xlab="Absolute",horiz=T)   
              }     
            }
          }
        }
      }
      # plot piechart if T
      if(piechart$plot){
        if(is.list(piechart)) {
          pos <- x[paste(unlist(str_split(splitter,pattern=",")))]
          for(m in 1:length(pos)){
            pos[[m]] <- as.character(pos[[m]])
          }      
          # order indicators
          pos <- as.data.table(unique(pos[-c(length(pos))]))
          setkeyv(x=pos,cols=colnames(pos))
          for(m in 1:nrow(pos)){
            temp <- x[eval(parse(text=paste(paste0("x$",splitnames),"==",pos[m,],collapse=" & "))),] 
            data = temp[,variables[i]]
            if(!(is.null(piechart$min)) && is.null(piechart$max)){        
              data <- ifelse(data>=piechart$min,data,NA)
            } 
            if((is.null(piechart$min)) && (!is.null(piechart$max))){          
              data <- ifelse(data<=piechart$max,data,NA)
            } 
            if(!(is.null(piechart$max)) && (!(is.null(piechart$min)))){         
              data <- ifelse(data<=piechart$max,data,NA)
              data <- ifelse(data>=piechart$min,data,NA)
            }
            if(!(is.null(piechart$missing))){
              if(piechart$missing){
                data <- table(x = data,useNA = "ifany")
                label <- c(names(attributes(x[,variables[i]])$value.labels),"NA")
                if(is.element(NA,names(data))){
                  pos <- which(is.element(names(data),NA))
                  names(data)[pos] <- "NA"
                  label <- c(names(attributes(x[,variables[i]])$value.labels),"NA")
                }
              }else{
                data <- table(x = data,useNA = "no")
                label <- names(attributes(x[,variables[i]])$value.labels)
              }
            }else{
              data <- table(x = data,useNA = "no")
              label <- names(attributes(x[,variables[i]])$value.labels)
            }
            
            if(!(is.null(piechart$percent)) & (is.null(piechart$freq))){
              if(piechart$percent){
                data <- data/sum(data)*100   
              } 
              if(length(data) == 0) {
                # minimum and maximum ranges equals data to zero.
                message("input data is empty. check maximum and minimum values")
              }
            }
            if(is.null(piechart$percent) & (!(is.null(piechart$freq)))){
              if(!(is.null(attributes(x[,variables[i]])$value.labels))){
                pie(data,main=attributes(x[,variables[i]])$variable.label,labels = label) 
              } else {
                pie(data,main=attributes(x[,variables[i]])$variable.label)    
              }
            }
            if((is.null(piechart$percent) & is.null(piechart$freq)) | ((!(is.null(piechart$percent))) & (!(is.null(piechart$freq))))){
              if(!(is.null(attributes(x[,variables[i]])$value.labels))){
                pie(data,main=attributes(x[,variables[i]])$variable.label,labels = label)    
              } else {
                pie(data,main=attributes(x[,variables[i]])$variable.label)   
              }     
            } else {
              stop("input data is empty. check maximum and minimum values")
            }
          }
        }
      }
      #######
    }else{
      
      # check if the variables contain value labels
      if(is.null(attributes(x[,variables[i]])$value.labels)){
        # no, unique and sort those variables
        val <- unique(sort(x[,variables[i]]))
        # placeholder for empty valuelabels
        vallab <- " "
      }else{
        # yes, unique and sort the names of the variables for valuelabels
        vallab <- names(sort(attributes(x[,variables[i]])$value.labels))
        # uniqe and sort the variablevalues
        val <- unique(sort(x[,variables[i]]))
      }
      
      
      # frequencies with NA
      freqWiNa <- c(table(x[,variables[i]],useNA="always"),sum(table(x[,variables[i]])))
      
      # NA indicator
      pos <- which(names(freqWiNa) %in% NA)
      # look after the position of the NA coloumn, check if it contains 0, if yes drop, if no keep
      if(freqWiNa[[pos]]==0){
        # drop the col
        freqWiNa <- freqWiNa[-pos]
      }
      if (is.element(NA, names(freqWiNa))) {
        # create the total amount of obs
        freqWiNa[length(freqWiNa)] <- as.numeric(freqWiNa[length(freqWiNa)]+freqWiNa[length(freqWiNa)-1])
      } 
      # create the total amount of perc
      perc <- c(prop.table(freqWiNa[-length(freqWiNa)]),sum(prop.table(freqWiNa[-length(freqWiNa)])))*100
      
      #frequencies without NA
      freqWoNa <- c(table(x[,variables[i]],useNA="no"),sum(table(x[,variables[i]])))
      # check if a NA coloumn is existent in freqWiNa
      if(is.element(NA,names(freqWiNa))){
        #yes calculate the valid percent with missing values like this
        validperc <- c(prop.table(freqWoNa[-length(freqWoNa)])*100,"Missing Value",sum(prop.table(freqWoNa[-length(freqWoNa)]))*100)  
      } else {
        #no calculate the valid percent without missing values like this
        validperc <- c(prop.table(freqWoNa[-length(freqWoNa)]),sum(prop.table(freqWoNa[-length(freqWoNa)])))*100  
      }
      #cumulated percent
      cumperc <- cumsum(validperc[-length(validperc)])
      # fill empty space with " "
      cumperc <- c(cumperc," ")
      # find na's
      pos <- which(cumperc %in% NA)
      # replace em with " "
      cumperc[pos] <- " "
      
      # check if the col length of value differs with freqWiNa
      if(length(val) != length(freqWiNa)){
        # check if its more then 1 row
        if((length(freqWiNa) - length(val)) >1) {
          # fill empty space with
          val <- c(val,"."," ")
        }
        # check if its exactly 1 row
        if((length(freqWiNa) - length(val)) ==1) {
          # fill empty space with
          val <- c(val," ")
        }
      }
      # check if the col length of vallab differs with freqWiNa
      if(length(vallab) != length(freqWiNa)){
        # check if its exactly 1 row
        if((length(freqWiNa) - length(vallab)) ==1) {
          # fill with
          vallab <- c(vallab," ")
        }
      }
      if(length(val) != length(vallab)){
        pos <- which(!(1:length(val) %in% 1:length(vallab)))
        vallab[pos] <- " "
        
      }
      
      # create frequencies output
      freq <-  cbind("ValueLabels"=vallab,
                     "Value"=val,
                     "Frequency"=freqWiNa,
                     "Percent"=perc,
                     "Valid Percent"=validperc,
                     "Cumulative Percentage"=cumperc)
      # create row names
      rownames(freq) <- seq(1:length(freq[,1]))
      # write total in the last row
      rownames(freq)[nrow(freq)] <- "Total"
      
      ################################ ---------------------------------------------------- ####################################
      
      #create placeholder for variables
      #     
      tempmean  <- tempmin <- tempmax <- tempmode <- tempmedian <-tempstddev <- tempkurtosis <- tempsekurtosis <- temprange <- tempsemean <- tempskewness <-      tempseskewness <- tempsum <- tempvariance <- NULL
      tempn <- N <- length(na.omit(x[,variables[i]]))
      if("mean" %in% statistics)      {
        tempmean <- mean(x[,variables[i]],na.rm=T)
      }
      if("median" %in% statistics) {
        tempmedian <- median(x[,variables[i]],na.rm=T)
      }
      if("minimum" %in% statistics) {
        tempmin <- min(x[,variables[i]],na.rm=T)
      }
      if("mode" %in% statistics) {
        tempmode <- max(table(x[,variables[i]]),na.rm=T)      
      }
      if("maximum" %in% statistics)      {
        tempmax<- max( x[,variables[i]],na.rm=T)
      }
      if("stddev" %in% statistics)      {      
        tempstddev <- sd(x[,variables[i]],na.rm=T)
      }
      if("kurtosis" %in% statistics)  {
        tempkurtosis <- kurtosis(x[,variables[i]],na.rm=T, type=2)   
        tempsekurtosis <- 2*(sqrt((6*N*(N-1)) / ((N-2)*(N+1)*(N+3))))* (sqrt(((N^2 -1)) / ((N-3)*(N+5))))
      }  
      if("range" %in% statistics)  {
        temprange <- diff(range(x[,variables[i]],na.rm=T))
      } 
      if("skewness" %in% statistics)  {
        tempskewness <- skewness(x[,variables[i]],na.rm=T, type=2)  
        tempseskewness <- sqrt((6*N*(N-1)) / ((N-2)*(N+1)*(N+3)))
      } 
      if("semean" %in% statistics)  {        
        tempsemean <- sd(na.omit(x[,variables[i]])/(sqrt(N)))   
      }   
      if("sum" %in% statistics)  {
        tempsum <- sum( x[,variables[i]],na.rm=T)    
      }   
      if("variance" %in% statistics)  {
        tempvariance <- var( x[,variables[i]],na.rm=T)    
      } 
      if("all" %in% statistics) {
        tempmax <- max(x[,variables[i]],na.rm=T)
        tempmean <- mean(x[,variables[i]],na.rm=T)
        tempmedian <- median(x[,variables[i]],na.rm=T)
        tempmin <- min(x[,variables[i]],na.rm=T)      
        tempmode <-  max(table(x[,variables[i]]),na.rm=T)
        tempstddev <- sd( x[,variables[i]],na.rm=T)
        tempkurtosis <- kurtosis( x[,variables[i]], type=2,na.rm=T)
        tempsekurtosis <-  sqrt(((N^2 -1)) / ((N-3)*(N+5)))
        temprange <- diff(range(x[,variables[i]],na.rm=T))
        tempsemean <- sd(x[,variables[i]],na.rm=T)/(sqrt(N))   
        tempskewness <-  skewness( x[,variables[i]], type=2,na.rm=T)
        tempseskewness  <- sqrt((6*N*(N-1)) / ((N-2)*(N+1)*(N+3)))
        tempsum  <-  sum( x[,variables[i]],na.rm=T)
        tempvariance  <- var( x[,variables[i]],na.rm=T)
      } 
      if("default" %in% statistics) {
        tempmean <- mean(x[,variables[i]],na.rm=T)
        tempmin <- min(x[,variables[i]],na.rm=T)
        tempmax <- max(x[,variables[i]],na.rm=T)
        tempstddev <- sd( x[,variables[i]],na.rm=T)
      }
      ntile <- NULL
      # calculate ntiles if the paramester is set
      if(is.null(ntiles) == F) {
        ntile <- list()
        temp <- 1/ntiles
        for(j in 1:length(ntiles)){
          ntile[[j]] <-  cbind(quantile(x[,variables[i]],probs= seq(from=0,to=1,by= temp[[j]]),na.rm=T))  
          names(ntile)[[j]] <- paste("ntile:", temp[[j]])
        }
      }    
      percentile <- NULL
      # calculate the percentiles if the parameter is set
      if(is.null(percentiles) == F) {
        # catch wrong input
        temp <- percentiles / 100    
        percentile <- quantile(x[,variables[i]],probs= temp,na.rm=T)
      }
      # create statistic output 
      descr <- cbind("n" = tempn,
                     "mean" = tempmean,
                     "minimum" = tempmin, 
                     "maximum" = tempmax, 
                     "stddev" = tempstddev,
                     "kurtosis" = tempkurtosis,
                     "sekurtosis" = tempsekurtosis,
                     "range" = temprange,
                     "semean" = tempsemean,
                     "skewness" = tempskewness,
                     "seskewness" = tempseskewness,
                     "sum" = tempsum,
                     "variance" = tempvariance)
      
      # combine statistics, freqs, ntiles and percintiles in a list
      
      frequencies[[i]] <- list("freqs" = freq,
                               "stats" = descr,
                               "ntiles" = ntile,
                               "percintile" = percentile)
      
      if(is.null(frequencies[[i]]$percintile)){
        frequencies[[i]]$percintile <- NULL
      }
      if(is.null(frequencies[[i]]$ntiles)){
        frequencies[[i]]$ntiles <- NULL
      }
      if("none" %in% statistics){
        frequencies[[i]]$stats <- NULL
      }
      
      # plot histogram if T
      if(histogram$plot){
        if(is.list(histogram)) {
          #calc parameters for curve
          data = x[,variables[i]]
          m<-mean(data,na.rm = T)
          std<-sqrt(var(data,na.rm = T))
          if(!(is.null(histogram$min)) && is.null(histogram$max)){         
            data <- ifelse(data>=histogram$min,data,NA)
          } 
          if((is.null(histogram$min)) && (!is.null(histogram$max))){       
            data <- ifelse(data<=histogram$max,data,NA)
          } 
          if(!(is.null(histogram$max)) && (!(is.null(histogram$min)))){         
            data <- ifelse(data<=histogram$max,data,NA)
            data <- ifelse(data>=histogram$min,data,NA)
          }
          if(!(is.null(histogram$freq))){
            if(histogram$freq < max(data)){
              stop("freq has to be equal or higher than the maximum value of the data")
            }
            if(!(is.null(histogram$normal))){
              if(histogram$normal){
                h <- hist(data,plot=F)
                hist(data,main=attributes(x[,variables[i]])$variable.label,xlab="Absolute",ylim=c(0,histogram$freq))
                multiplier <- h$counts / h$density
                mydensity <- density(data,na.rm=T)
                mydensity$y <- mydensity$y * multiplier[1]
                lines(mydensity)
              }else{
                hist(data,main=attributes(x[,variables[i]])$variable.label,col=1:length(unique(data)),ylim=c(0,histogram$freq),xlab="Absolute")    
              }
            } 
          }        
          if(!(is.null(histogram$normal))){
            if(histogram$normal){
              h <- hist(data,plot=F)
              hist(data,main=attributes(x[,variables[i]])$variable.label,xlab="Absolute",ylim=c(0,max(h$counts*1.5)))
              multiplier <- h$counts / h$density
              mydensity <- density(data,na.rm=T)
              mydensity$y <- mydensity$y * multiplier[1]
              lines(mydensity)
            }else{
              hist(data,main=attributes(x[,variables[i]])$variable.label,xlab="Absolute")    
            }
          }
          if(is.null(histogram$normal) & is.null(histogram$freq)){
            hist(data,main=attributes(x[,variables[i]])$variable.label,ylab="Absolute")              
          }
        }
      }
      # plot barchart if T
      if(barchart$plot){
        if(is.list(barchart)) {
          data = x[,variables[i]]
          if(!(is.null(barchart$min)) && is.null(barchart$max)){        
            data <- ifelse(data>=barchart$min,data,NA)
          } 
          if((is.null(barchart$min)) && (!is.null(barchart$max))){          
            data <- ifelse(data<=barchart$max,data,NA)
          } 
          if(!(is.null(barchart$max)) && (!(is.null(barchart$min)))){         
            data <- ifelse(data<=barchart$max,data,NA)
            data <- ifelse(data>=barchart$min,data,NA)
          }
          data <- table(data)
          if(!(is.null(barchart$percent)) & is.null(barchart$freq)){
            if(barchart$percent != F){
              data <- data/sum(data)*100
              if(barchart$percent < max(data)){
                stop("percent has to be equal or higher than the maximum value of the data")
              }
              if(!(is.null(attributes(x[,variables[i]])$value.labels))){
                barplot(data,main=attributes(x[,variables[i]])$variable.label,ylim=c(0,barchart$percent),legend = names(attributes(x[,variables[i]])$value.labels),col=1:length(attributes(x[,variables[i]])$value.labels),ylab="Percent")    
              } else{
                barplot(data,main=attributes(x[,variables[i]])$variable.label,ylim=c(0,barchart$percent),col=1:length(attributes(x[,variables[i]])$value.labels),ylab="Percent")    
              }            
            }
          }
          if(is.null(barchart$percent) & (!(is.null(barchart$freq)))){
            if(barchart$freq < max(data)){
              stop("freq has to be equal or higher than the maximum value of the data")
            }
            if(!(is.null(attributes(x[,variables[i]])$value.labels))){
              barplot(data,main=attributes(x[,variables[i]])$variable.label,ylim=c(0,barchart$freq),legend = names(attributes(x[,variables[i]])$value.labels),col=1:length(attributes(x[,variables[i]])$value.labels),ylab="Absolute")    
            } else {
              barplot(data,main=attributes(x[,variables[i]])$variable.label,ylim=c(0,barchart$freq),col=1:length(attributes(x[,variables[i]])$value.labels),ylab="Absolute")    
            }
          }
          if(is.null(barchart$percent) & is.null(barchart$freq)){
            if(!(is.null(attributes(x[,variables[i]])$value.labels))){
              barplot(data,main=attributes(x[,variables[i]])$variable.label,legend = names(attributes(x[,variables[i]])$value.labels),col=1:length(attributes(x[,variables[i]])$value.labels),ylab="Absolute")    
            } else {
              barplot(data,main=attributes(x[,variables[i]])$variable.label,col=1:length(attributes(x[,variables[i]])$value.labels),ylab="Absolute")   
            }     
          }
        }
      }
      # plot piechart if T
      if(piechart$plot){
        if(is.list(piechart)) {
          data = x[,variables[i]]
          if(!(is.null(piechart$min)) && is.null(piechart$max)){        
            data <- ifelse(data>=piechart$min,data,NA)
          } 
          if((is.null(piechart$min)) && (!is.null(piechart$max))){          
            data <- ifelse(data<=piechart$max,data,NA)
          } 
          if(!(is.null(piechart$max)) && (!(is.null(piechart$min)))){         
            data <- ifelse(data<=piechart$max,data,NA)
            data <- ifelse(data>=piechart$min,data,NA)
          }
          if(!(is.null(piechart$missing))){
            if(piechart$missing){
              data <- table(x = data,useNA = "ifany")
              label <- c(names(attributes(x[,variables[i]])$value.labels),"NA")
              if(is.element(NA,names(data))){
                pos <- which(is.element(names(data),NA))
                names(data)[pos] <- "NA"
                label <- c(names(attributes(x[,variables[i]])$value.labels),"NA")
              }
            }else{
              data <- table(x = data,useNA = "no")
              label <- names(attributes(x[,variables[i]])$value.labels)
            }
          }else{
            data <- table(x = data,useNA = "no")
            label <- names(attributes(x[,variables[i]])$value.labels)
          }
          
          if(!(is.null(piechart$percent)) & (is.null(piechart$freq))){
            if(piechart$percent){
              data <- data/sum(data)*100   
            } 
            if(length(data) == 0) {
              # minimum and maximum ranges equals data to zero.
              message("input data is empty. check maximum and minimum values")
            }
          }
          if(is.null(piechart$percent) & (!(is.null(piechart$freq)))){
            if(!(is.null(attributes(x[,variables[i]])$value.labels))){
              pie(data,main=attributes(x[,variables[i]])$variable.label,labels = label) 
            } else {
              pie(data,main=attributes(x[,variables[i]])$variable.label)    
            }
          }
          if((is.null(piechart$percent) & is.null(piechart$freq)) | ((!(is.null(piechart$percent))) & (!(is.null(piechart$freq))))){
            if(!(is.null(attributes(x[,variables[i]])$value.labels))){
              pie(data,main=attributes(x[,variables[i]])$variable.label,labels = label)    
            } else {
              pie(data,main=attributes(x[,variables[i]])$variable.label)   
            }     
          } else {
            stop("input data is empty. check maximum and minimum values")
          }
        }
      }
    }
    names(frequencies)[i] <- variables[i]
    frequencies <- noquote(frequencies)
  }
  
  if(!(is.xpssFrame(x))){
    attributes(x)$SPLIT_FILE <- NULL
  }
  # set ask to default F
  options(warn=0)
  #par(ask=F)
  
  return(frequencies)
}
