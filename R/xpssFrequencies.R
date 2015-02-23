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
#' \cr \tab \code{freq=n} \tab Displays the distrubtion in absolute values on the basis of a user-defined maxima, the maxima has to be higher then the maxima of the distribtion, freq=max(n) is the default. (except  for piechart).
#' \cr \tab \code{percent=n} \tab Displays the distrubtion in relative values on the basis of a user-defined maxima, the maxima has to be higher then the maxima of the distribtion, percent=max(n) is the default. (except  for piechart).
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
#'\cr \tab \code{mean} \tab calculates the arithmetic mean, respectively the midpoint of the variable.
#'\cr \tab \code{median} \tab calculates the arithmetic mean, respectively the midpoint of the variable.
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
#' @param barchart plot a barchart. Default for \code{plot} is \code{NULL}. If \code{plot} is \code{TRUE} an default barchart will be plotted. Optional for customized barchart a list with the following arguments has to be assigned \code{minimum}, \code{maximum} to bound lower and upper values which are not plotted. See notes for more.
#' @param piechart plot a piechart. Default for \code{plot} is \code{NULL}. If \code{plot} is \code{TRUE} an default an default piechart will be plotted. Optional for customized piechart a list with the following arguments has to be assigned \code{minimum}, \code{maximum} to bound lower and upper values which are not plotted. See notes for more.
#' @param histogram plot a histogram.  Default for \code{plot} is \code{NULL}. If \code{plot} is \code{TRUE} an default histogram will be plotted. Optional for customized histogram a list with the following arguments has to be assigned \code{minimum}, \code{maximum(n)} to bound lower and upper values which are not plotted. With \code{normal} a overlapping normal distrubtion line will drawn. Default is \code{FALSE}.  See notes for more.
#' @param ntiles divides the distribution in a specific percentage amount of categories. multiple dividing in distributions is allowed. Default is \code{NULL}.
#' @param percentiles displays the value between customized percentiles. Default is \code{NULL}.
#' @param statistics Method which enumerate the deskriptive statistics. Default is \code{mean}, \code{stddev}, \code{minimum}, \code{maximum}. Optional arguments are \code{all},\code{kurtosis}, \code{median}, \code{mode}, \code{none}, \code{range}, \code{sekurt}, \code{semean}, \code{seskew}, \code{skewness}, \code{sum}, \code{variance}.
#' @importFrom e1071 kurtosis skewness
#' @importFrom graphics pie hist barplot
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
                             variables = NULL,
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
  par(ask=T)
  options(warn=-1)
  
  # exception check if the named variables are in the dataset
  for(i in 1:length(variables)){
    if(!(is.element(variables[i],names(x)))) {
      stop("The selected variables are not in the dataset")
    }
  }
  
  
  # define function type
  functiontype <- "AN"
  # do meta check
  x <- applyMetaCheck(x)
  
  
  ## if missing is include 
  
  # include missing values
  if("include" %in% missing)
  {
    # select the variables
    temp <- computeValue(x,variables)
    # get the position of the variables
    pos <- which(colnames(temp) %in% variables)
    for(i in 1:length(variables))
    {
      #w rite the user defined missings back in the actual data
      x[,variables[i]] <- temp[,pos[i]]
    }
  }  
  # placeholder for output
  frequencies <- list()
  #global loop
  for(i in 1:length(variables)) {
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
    
    tempmean <- NULL
    tempmin <- NULL
    tempmax <- NULL
    tempmode <- NULL
    tempmedian <- NULL
    tempstddev  <- NULL
    tempkurtosis <- NULL
    tempsekurtosis <- NULL
    temprange <- NULL
    tempsemean <- NULL
    tempskewness <- NULL
    tempseskewness <- NULL
    tempsum <- NULL
    tempvariance <- NULL 
    tempn <- N <- length(na.omit(x[,variables[i]]))
    if("mean" %in% statistics)      {
      tempmean <- mean(x[,variables[i]],na.rm=T)
    }
    if("median" %in% statistics) {
      tempmean <- median(x[,variables[i]],na.rm=T)
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
      tempmean <- median(x[,variables[i]],na.rm=T)
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
      for(j in 1:length(ntiles)){
        # catch wrong input
        if(ntiles[[j]] > 1 && ntiles[[j]] < 0){
          # yes, throw exception
          stop("valid value are 0 - 1")
        } else {
          # no, calculate the tiles
          ntile[[j]] <-  cbind(quantile(x[,variables[i]],probs= seq(0,1, ntiles[[j]]),na.rm=T))
        }      
      }  
    }    
    percentile <- NULL
    # calculate the percentiles if the parameter is set
    if(is.null(percentiles) == F) {
      percentile <- NULL
      # catch wrong input
      if(percentiles > 1 && percentiles < 0){
        # yes, throw exception
        stop("valid value are 0 - 1")
      } else {
        # no, calculate the percentiles
        percentile <-  cbind(quantile(x[,variables[i]],probs= percentiles,na.rm=T))
      }      
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
  # set ask to default F
  options(warn=0)
  par(ask=F)
  names(frequencies) <- variables
  frequencies <- noquote(frequencies)
  return(frequencies)
}
