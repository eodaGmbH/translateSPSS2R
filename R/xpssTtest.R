#' Performs a T-Test
#'
#' R Implementation of the SPSS \code{T-TEST} Function
#'
#' Performs a student T-Test. The \code{xpssT.test} compares the mean by calculating Students-t of the selected distributions.
#' 
#' It is possible to check the samples
#' 
#' \enumerate{
#' \item against a specific value (one-sample) -> \code{testval}
#' \item in difference of groups (independend-sample)-> \code{groups}
#' \item in difference of variables (paired-sample) -> \code{pairs}
#'  }
#' 
#' Simple statistics will be printed with every t-test.
#' At the one-sample test, the mean difference will be visualized with the statistics \cr
#' At the independend-sample test, the mean differnce and ANOVA will be visualized with the statistics \cr
#' At the paired-sample test, the mean diference and a Correlation statistic will be visualizied witht he statistics \cr
#'
#' @param x a (non-empty) data.frame, data.table object or input data of class \code{"xpssFrame"}. 
#' @param variables atomic character or character vektor with the name of the variables.
#' @param t_test defines the type of t-test. Default is \code{testval}, a one-sample ttest. 
#' Optional arguments are \code{groups} for an independent-sample test and \code{pairs} for an paires-sample test 
#' @param testval determines the value of mean difference.
#' @param criteria specifies the confidence interval for the mean differences. Default
#' is "0.95", optionally a customized value between 0 and 1 can be used.
#' @param groupvar atomic character with the name of the variables which shall be used for grouping.
#' @param groups specifies one variable which get grouped for an independentsample t-test.
#' @param paired logical, indicating if the comparing should be pair based or not.
#' @param withvars atomic characters or character vektor with the name of the
#' paired variables which shall be used for compare the means. Optionally
#' the argument with can be chosen to compare the means of 2 pairs.
#' @param missing method which indicates what should happen when the data contains NAs. Default is \code{analysi}s".
#'  Optionally \code{include} oder \code{listwise} can be used.
#' @return returns a list depending upon the t-test.
#' 
#'  All t-test contain: \cr
#' \tabular{rlll}{
#' 
#' \tab \code{statistics} \tab  simple statistics. \cr
#' \tab \code{parameter} \tab degrees of freedom. \cr
#' \tab \code{p.value} \tab significance niveau. \cr
#' \tab \code{conf.int} \tab confidence bound. \cr
#' \tab \code{null.value} \tab value of null hypothesis. \cr
#' \tab \code{alternative} \tab value of alternative hypothesis. \cr
#' \tab \code{method} \tab character string with the name of the t-test. \cr
#' \tab \code{data.name} \tab name of the data.} \cr \cr
#' 
#' The independent t-test includes additonally: \cr
#' \tabular{rlll}{
#' 
#' \tab \code{anova} \tab anova of the groups.} \cr \cr
#' 
#' The paired t-test includes additonally: \cr
#' \tabular{rlll}{
#' 
#' \tab \code{corr} \tab correlation of the pairs.} \cr \cr
#' 
#' @author Bastian Wiessner
#' 
#' @examples
#' data(fromXPSS)
#' 
#' temp <- xpssTtest(fromXPSS,
#'                    variables = "V7_2", 
#'                    t_test = "testval", 
#'                    testval= 50, 
#'                    criteria = 0.65)
#'                    
#' temp <- xpssTtest(fromXPSS,
#'                    variables = "V7_2",
#'                    t_test = "groups",
#'                    groupvar = "V3",
#'                    groups = c(1,
#'                               2),
#'                    missing = "analysis",
#'                    criteria = 0.99)
#'                    
#' temp <- xpssTtest(fromXPSS,
#'                    t_test = "pairs",
#'                    variables = c("V5",
#'                                  "V7_1"),
#'                    withvars = "V7_2",
#'                    missing = "include",
#'                    criteria = 0.99)
#'                    
#' temp <- xpssTtest(fromXPSS,
#'                    t_test = "pairs",
#'                    variables=c("V5",
#'                                "V6"),
#'                    withvars = c("V7_2",
#'                                 "V7_1"),
#'                    paired = TRUE,
#'                    missing = "listwise")
#'                    
#' temp <- xpssTtest(fromXPSS,
#'                    t_test = "pairs",
#'                    variables=c("V5",
#'                                "V6",
#'                                "V7_1",
#'                                "V7_2"), 
#'                    missing = "analysis",
#'                    criteria = 0.99)
#'                    
#' temp <- xpssTtest(fromXPSS,
#'                    t_test = "pairs",
#'                    variables=c("V5",
#'                                "V6",
#'                                "V7_1",
#'                                "V7_2"), 
#'                    paired = T,
#'                    missing = "analysis",
#'                    criteria = 0.85)
#' @export
xpssTtest <- function(x,
                       variables = NULL,
                       t_test = "testval",
                       testval = NULL,
                       criteria = 0.95,
                       groupvar = NULL,
                       groups = NULL,
                       withvars = NULL,
                       paired = FALSE,
                       missing = "analysis") 
  {
  
  library(car)
  library(plyr)
  
  t.val <- list(variables[[1]]) 
  t.with <- list(withvars[[1]])
  t.group <- list()
  logvec_withvars <- list()
  logvec <- list()
  myt <- list()
  t.out <- list(variables[[1]]) 
  t.one <- list()
  t.cor <- list()
  f.out <- list()
  erg <- list()
  
  stopifnot(is.data.frame(x) | is.data.table(x) | class(x) == "xpssFrame")
  if(is.null(variables))
  {
    stop("argument variables is missing, no default available")
  }
  for(i in 1:length(variables))  {
    if(class(x[,variables[i]]) != "numeric"){  
      stop("variables are not numeric")
    } 
  }
  if(t_test != "testval" && t_test != "groups" && t_test != "pairs")  {
    stop("argument 't_test' is wrong. Only the arguments 'testval', 'groups', and 'pairs' are valid.")
  }
  if(t_test == "testval")  {
    if(is.null(testval)){
      stop("argument 'testval' is missing, no default available")
    } 
    if(class(testval) != "numeric"){
      stop("argument 'testval' is not numeric")
    }
    if(is.null(groupvar) ==F || is.null(groups) ==F || is.null(withvars) ==F)
    {
      warning("paired or grouped t-test's aren't possible at the one sample t-test")  
    }
  }
  if(t_test == "groups")  {
    if(is.null(groupvar)){
      stop("argument 'groups' is missing, no default available")
    }
    if(length(groups) > 2 || length(groups) < 2)      {
        stop("grouping factor must have exactly 2 levels")
    }  
    if(is.null(testval) ==F || is.null(withvars) ==F)
    {
      warning("one sample t-tests or paired t-test's aren't possible at indenpendent t-test's")  
    }
  }
  if(t_test == "pairs")  {
    if(is.null(x[,withvars]) == T)  {
      for(i in 1:length(x[,withvars]))    {
        if(class(x[,withvars[i]]) != "numeric"){  
          stop("withvariables are not numeric")
        } 
      }
    }
    if(is.null(testval) ==F || is.null(groupvar) ==F || is.null(groups) ==F)
    {
      warning("one sample t-tests or independent t-test's aren't possible at paired t-test's")  
    }
  }
  if(class(criteria) != "numeric"){  
    stop("argument 'criteria' is not numeric")
  }
  if(criteria >1 || criteria <0)
  {
    stop("valid arguments for 'criteria' are only single numbers between 0 and 1")
  }
  if(missing != "analysis" && missing != "listwise" && missing != "include")  {
    stop("wrong 'missing' argument. Only the arguments 'analysis', 'listwise', and 'include' are valid.")
  }
#   
#   
#   
#   if(class(groups) == "character")
#   {
#     groups[[1]]
#   }
#             
options(warn=-1) 
  #-----------------------------------------------------------#
  #--------------missings-----------------------
  if("analysis" %in% missing)  {
    if(is.null(withvars) && is.null(groupvar))    {
      for(i in 1:length(variables))      {
        logvec[[i]] <- is.na(x[,variables[i]]) 
        t.val[[i]] <- x[,variables[i]]
      }      
    }
    else {
      if(is.null(withvars) == FALSE) {
         k <- 1
         for(i in 1:length(variables)) {
           for(j in 1:length(withvars))  {
             logvec[[k]] <- (is.na(x[,variables[i]]) | (is.na(x[,withvars[j]])))
             t.val <- x[,variables]
             t.with <- x[,withvars]
             k <- k+1
           }
         }
      }
      if(is.null(groupvar) == FALSE){
        for(i in 1:length(variables))  {
          logvec[[i]] <- (is.na(x[,variables[i]]) | is.na(x[,groupvar]))
          t.val[[i]] <- x[,variables[i]]
        }
      }
    }
  }
    
  if("listwise" %in% missing)  {
    if(is.null(withvars))    {
      for(i in 1:length(variables))      {
        t.val[[i]] <- x[,variables[i]]
      }
    } else {
     for(i in 1:length(variables))     {
       t.val[[i]] <- x[,variables[i]]
     }
     for(i in 1:length(withvars))     {
       t.with[[i]] <- x[,withvars[i]]
     }
    }
  } 
  if("include" %in% missing)  {    
    if(is.null(withvars))    {
      x[,variables] <- xpssValue(x,variables)      
      for(i in 1:length(variables))      {
        logvec[[i]] <- is.na(x[,variables[i]])  
        t.val[[i]] <- x[,variables[i]]
      }
    } else {
      temp <- xpssValue(x,variables)
      
      pos <- which(names(temp) %in% variables)
            
      for(i in 1:length(variables)) {
        logvec[[i]] <- is.na(temp[,pos[i]])  
        t.val[[i]] <- temp[,pos[i]]
      }
        
      temp <- xpssValue(x,withvars)
      pos <- which(names(temp) %in% withvars)
      for(i in 1:length(withvars)) {
        logvec_withvars[[i]] <- is.na(temp[,pos[i]])  
        t.with[[i]] <- temp[,pos[i]]
      }
    }
  }
#--------------------------tests------------------
 #EinStichprobenT_Test
 
 if(t_test == "testval") {
   for(i in 1:length(variables))   {
     if(missing == "listwise")     {
       logvec <- complete.cases(t.val)
       pos <- which(logvec%in%T)
     }
     if(missing == "include") {
       pos <- which(logvec[[i]]%in%F)
     } 
     if(missing == "analysis")  {
       pos <- which(logvec[[i]]%in%F)
     }
        
    t.one[[i]] <- t.test(t.val[[i]][pos], mu = testval,conf.level=criteria)
    t.one[[i]]$conf.int <- t.one[[i]]$conf.int - testval
    t.one[[i]]$estimate <-t.one[[i]]$estimate-10
    t.one[[i]]$data.name <- paste(attr(x[,variables[i]],"variable.label")[1])
   
    t.one[[i]]$observation  <- length(t.val[[i]][pos])
    t.one[[i]]$mean <- mean(t.val[[i]][pos])
    t.one[[i]]$sd <- sd(t.val[[i]][pos])
    t.one[[i]]$semean <- (t.one[[i]]$sd /(sqrt(t.one[[i]]$observation))) 
    
    myt[[i]] <- list("n" = t.one[[i]]$observation,
                    "mean" =  t.one[[i]]$mean,
                    "sd" = t.one[[i]]$sd,
                    "semean" = t.one[[i]]$semean)
    
    t.out <- list("t-Value" =  t.one[[i]]$statistic[[1]], 
                  "significance Level" =  t.one[[i]]$p.value[[1]],
                  "df" =  t.one[[i]]$parameter[[1]],
                  "mean difference" = t.one[[i]]$mean - testval,
                  "lower confidence level of difference" = t.one[[i]]$conf.int[[1]],
                  "upper confidence level of difference" = t.one[[i]]$conf.int[[2]])
#     
#   erg[[i]] <- list("descriptive" =myt[[i]],
#                     "t.test" =t.out)
#   

    erg[[i]] <- list(t.one[[i]])
    names(erg[[i]])  <- t.one[[i]]$data.name
   }
 }
 #Gruppiert
 if(t_test == "groups") {
   k <- 1
   for(i in 1:length(variables))   {
     if(missing == "include")     {
       grp <- which((x[,groupvar] == groups[1]) | (x[,groupvar] == groups[2]) & (logvec[[i]]%in%F))
     } else {
       grp <- which((x[,groupvar] == groups[1]) | (x[,groupvar] == groups[2]))
     }
     if(length(grp) < 4)
     {
       stop("not enough observations for a grouped t-test available, the minimum of observations are 4")
     }
       #Gruppenstatistik unabhängig
      
       #t.val[[i]] <- t.test(t.val[[i]][pos] ~ x[,groupvar][pos],var.equal=T,conf.level=criteria)
       t.one[[i]] <- t.test(t.val[[i]][grp] ~  x[,groupvar][grp]  ,var.equal=F,conf.level=criteria)
       t.one[[i]]$data.name  <- paste(attr(t.val[[i]],"variable.label")[1],"by", attr(x[,groupvar],"variable.label")[1])

       t.one[[i]]$observation  <- list(length(na.omit(t.val[[i]][which(x[,groupvar] %in% 1)])),length(na.omit(t.val[[i]][which(x[,groupvar] %in% 2)])))
      t.one[[i]]$mean <- list(mean(na.omit(t.val[[i]][which(x[,groupvar] %in% 1)])),mean(na.omit(t.val[[i]][which(x[,groupvar] %in% 2)])))
       t.one[[i]]$sd <- list(sd(na.omit(t.val[[i]][which(x[,groupvar] %in% 1)])),sd(na.omit(t.val[[i]][which(x[,groupvar] %in% 2)])))
       t.one[[i]]$semean <- list(t.one[[i]]$sd[[1]] /(sqrt(t.one[[i]]$observation[[1]])),t.one[[i]]$sd[[2]] /(sqrt(t.one[[i]]$observation[[2]])))  
 
     #Stichprobenbstatistik
     
     t.group[[i]] <- leveneTest(t.val[[i]][grp], g = x[,groupvar][grp], center="mean")
     #t.group[[i]]$data.name <- paste(attr(t.val[[i]],"variable.label")[1],"by", attr(x[,groupvar],"variable.label")[1])
     t.group[[i]]$names <- list(names(which(attr(x[,groupvar],"value.labels") == groups[[1]])),names(which(attr(x[,groupvar],"value.labels") == groups[[2]])))         
     
     t.cor[[i]] <- anova(lm(t.val[[i]]~1))
     
     myt[[i]] <- list("Gruppe1" = list("n" = t.one[[i]]$observation[[1]],
                      "mean" =  t.one[[i]]$mean[[1]],
                      "sd" = t.one[[i]]$sd[[1]],
                      "semean" = t.one[[i]]$semean[[1]]),
                      "Gruppe2" = list("n" = t.one[[i]]$observation[[2]],
                           "mean" =  t.one[[i]]$mean[[2]],
                           "sd" = t.one[[i]]$sd[[2]],
                           "semean" = t.one[[i]]$semean[[2]]))
     names(myt[[i]]) <- t.group[[i]]$names
                  
     t.out <- list("T-Value" =  t.one[[i]]$statistic[[1]], 
                        "Significance Level" =  t.one[[i]]$p.value, 
                        "df" =  t.one[[i]]$parameter[[1]], 
                        "Difference in Mean"= t.one[[i]]$estimate[1][[1]]-t.one[[i]]$estimate[2][[1]])
     
     f.out <- list("F-Value" = t.group[[i]][[2]][1],
                   "Significance Level" =t.group[[i]][[3]][1],
                   "Sum Sq" = t.cor[[i]][[2]][1],
                   "Mean Sq" = t.cor[[i]][[3]][1])
#        
#      erg[[i]] <- list("descriptive" =myt[[i]],
#                       "t.test" =t.out,
#                       "anova" =f.out)

erg[[i]] <- list(t.one[[i]])
names(erg[[i]])  <- t.one[[i]]$data.name
                      
  }
 }
 #pairs

 if(t_test == "pairs" && is.null(withvars)) {
   k <- 1
   for(i in 1:(length(variables)-1))   {
     for(j in 2:length(variables))     {
       if(missing == "listwise")       {
        logicalvec <- complete.cases(x[,variables])
       } else {
         temp <- c(logvec[i],logvec[j])
         logicalvec <- temp[[1]] == temp[[2]]         
       }     
         if(j>i){ 
          pos <- which(logicalvec%in%T)
          t.one[[k]] <- t.test(t.val[[i]][pos],t.val[[j]][pos],paired=T,conf.level=criteria)
          t.one[[k]]$data.name  <- paste(attr(t.val[[i]],"variable.label")[1],"with", attr(t.val[[j]],"variable.label")[1])
          #t.one[[k]]$data.name  <- list(attr(x[,variables[i]],"variable.label")[1],attr(t.val[[j]],"variable.label")[1])
            
          ###Statistiken
          
          t.one[[k]]$observation <- c(length(t.val[[i]][pos]),length(t.val[[j]][pos]))
          t.one[[k]]$mean <- c(mean(na.omit(t.val[[i]][pos])),mean(na.omit(t.val[[j]][pos])))
          t.one[[k]]$sd <- c(sd(na.omit(t.val[[i]][pos])),sd(na.omit(t.val[[j]][pos])))
          t.one[[k]]$semean <- c(t.one[[i]]$sd[1] /(sqrt(t.one[[i]]$observation[1])),t.one[[i]]$sd[2] /(sqrt(t.one[[i]]$observation[2])))

          ####Korrelationen
          
          t.cor[[k]] <-cor.test(na.omit(t.val[[i]][pos]),na.omit(t.val[[j]][pos]))
          
          myt[[k]] <- list("Gruppe1" = list("n" = t.one[[k]]$observation[[1]],
                                            "mean" =  t.one[[k]]$mean[[1]],
                                            "sd" = t.one[[k]]$sd[[1]],
                                            "semean" = t.one[[k]]$semean[[1]]),
                           "Gruppe2" = list("n" = t.one[[k]]$observation[[2]],
                                            "mean" =  t.one[[k]]$mean[[2]],
                                            "sd" = t.one[[k]]$sd[[2]],
                                            "semean" = t.one[[k]]$semean[[2]]))
          names(myt[[k]]) <-  t.one[[k]]$data.name
          
          t.cor <- list("Pair" = k,
                        "N" = t.one[[k]]$observation[[1]],
                        "correlation" = t.cor[[k]]$estimate[[1]],
                        "sig" = t.cor[[k]]$p.value)
          
          t.out <- list("T-Value" =  t.one[[k]]$statistic[[1]], 
                        "sig" =  t.one[[k]]$p.value, 
                        "df" =  t.one[[k]]$parameter[[1]])
#           erg[[k]] <- list(myt[[k]],
#                            "correlation" = t.cor,
#                            "t.test" =t.out)
          
erg[[k]] <- list(t.one[[k]])
names(erg[[k]])  <- t.one[[k]]$data.name
          k <- k+1
         }
      }
    }
  }
 if(t_test == "pairs" &&  !is.null(withvars) && paired == F) {
   k <- 1
   for(i in 1:(length(variables)))   {
     for(j in 1:length(withvars))     {
       
       if(missing == "listwise")       {
         temp <- c(t.val,t.with)
         logvec <- complete.cases(temp)
         pos <- which(logvec%in%T)
       }
       if(missing == "include")       {
         temp <- c(logvec[i],logvec_withvars[j])
         logicalvec <- temp[[1]] == temp[[2]]
         pos <- which(logicalvec%in%T)
       } 
       if(missing == "analysis")       {
         pos <- which(logvec[[k]]%in%F)
       }
         t.one[[k]] <- t.test(t.val[[i]][pos],t.with[[j]][pos],paired=T,conf.level=criteria)
         t.one[[k]]$data.name  <- paste(attr(t.val[[i]],"variable.label")[1],"with", attr(t.with[[j]],"variable.label")[1])
         #t.one[[k]]$data.name  <- list(attr(x[,variables[i]],"variable.label")[1],attr(x[,withvars[j]],"variable.label")[1])
         
         ###Statistiken
         
         t.one[[k]]$observation <- c(length(t.val[[i]][pos]),length(t.with[[j]][pos]))
         t.one[[k]]$mean <- c(mean(na.omit(t.val[[i]][pos])),mean(na.omit(t.with[[j]][pos])))
         t.one[[k]]$sd <- c(sd(na.omit(t.val[[i]][pos])),sd(na.omit(t.with[[j]][pos])))
         t.one[[k]]$semean <- c(t.one[[i]]$sd[1] /(sqrt(t.one[[i]]$observation[1])),t.one[[i]]$sd[2] /(sqrt(t.one[[i]]$observation[2])))
         
         t.cor[[k]] <-cor.test(t.val[[i]][pos],t.with[[j]][pos])
         
         myt[[k]] <- list("Gruppe1" = list("n" = t.one[[k]]$observation[[1]],
                                           "mean" =  t.one[[k]]$mean[[1]],
                                           "sd" = t.one[[k]]$sd[[1]],
                                           "semean" = t.one[[k]]$semean[[1]]),
                          "Gruppe2" = list("n" = t.one[[k]]$observation[[2]],
                                           "mean" =  t.one[[k]]$mean[[2]],
                                           "sd" = t.one[[k]]$sd[[2]],
                                           "semean" = t.one[[k]]$semean[[2]]))
         names(myt[[k]]) <-  t.one[[k]]$data.name
         
         t.cor <- list("Pair" = k,
                       "N" = t.one[[k]]$observation[[1]],
                       "correlation" = t.cor[[k]]$estimate[[1]],
                       "sig" = t.cor[[k]]$p.value)
         
         t.out <- list("T-Value" =  t.one[[k]]$statistic[[1]], 
                       "sig" =  t.one[[k]]$p.value, 
                       "df" =  t.one[[k]]$parameter[[1]])
         
         #           erg[[k]] <- list(myt[[k]],
         #                            "correlation" = t.cor,
         #                            "t.test" =t.out)
         
       }
       erg[[k]] <- list(t.one[[k]])
       names(erg[[k]])  <- t.one[[k]]$data.name
       k <- k+1
     }
   }

if(t_test == "pairs" &&  !is.null(withvars) && paired == T)
{
  if(missing == "listwise")       {
    temp <- c(t.val,t.with)
    logvec <- complete.cases(temp)
    pos <- which(logvec%in%T)
  }
  if(missing == "include")       {
    temp <- c(logvec[i],logvec_withvars[j])
    logicalvec <- temp[[1]] == temp[[2]]
    pos <- which(logicalvec%in%T)
  } 
  if(missing == "analysis")       {
    pos <- which(logvec[[k]]%in%F)
}
  if(length(t.val) == length(t.with))
  {
    for(i in 1:length(variables))
    {
      t.one[[i]] <- t.test(t.val[[i]][pos],t.with[[i]][pos],paired=T,conf.level=criteria)
      t.one[[i]]$data.name  <- paste(attr(t.val[[i]],"variable.label")[1],"with", attr(t.with[[i]],"variable.label")[1])
      #t.one[[i]]$data.name  <- list(attr(x[,variables[i]],"variable.label")[1],attr(x[,withvars[i]],"variable.label")[1])
      
      ###Statistiken
      
      t.one[[i]]$observation <- c(length(t.val[[i]][pos]),length(t.with[[i]][pos]))
      t.one[[i]]$mean <- c(mean(na.omit(t.val[[i]][pos])),mean(na.omit(t.with[[i]][pos])))
      t.one[[i]]$sd <- c(sd(na.omit(t.val[[i]][pos])),sd(na.omit(t.with[[i]][pos])))
      t.one[[i]]$semean <- c(t.one[[i]]$sd[1] /(sqrt(t.one[[i]]$observation[1])),t.one[[i]]$sd[2] /(sqrt(t.one[[i]]$observation[2])))
      
      t.cor[[i]] <-cor.test(t.val[[i]][pos],t.with[[i]][pos])
      
      myt[[i]] <- list("Gruppe1" = list("n" = t.one[[i]]$observation[[1]],
                                        "mean" =  t.one[[i]]$mean[[1]],
                                        "sd" = t.one[[i]]$sd[[1]],
                                        "semean" = t.one[[i]]$semean[[1]]),
                       "Gruppe2" = list("n" = t.one[[i]]$observation[[2]],
                                        "mean" =  t.one[[i]]$mean[[2]],
                                        "sd" = t.one[[i]]$sd[[2]],
                                        "semean" = t.one[[i]]$semean[[2]]))
      names(myt[[i]]) <-  t.one[[i]]$data.name
      
      t.cor <- list("Pair" = i,
                    "N" = t.one[[i]]$observation[[1]],
                    "correlation" = t.cor[[i]]$estimate[[1]],
                    "sig" = t.cor[[i]]$p.value)
      
      t.out <- list("T-Value" =  t.one[[i]]$statistic[[1]], 
                    "sig" =  t.one[[i]]$p.value, 
                    "df" =  t.one[[i]]$parameter[[1]])
      
      #           erg[[i]] <- list(myt[[i]],
      #                            "correlation" = t.cor,
      # 
      erg[[i]] <- list(t.one[[i]])
      names(erg[[i]])  <- t.one[[i]]$data.name
    }
  } else {
    stop("No the same amount of withvars and variables")
  }
}
options(warn=0)
return(erg)
 }
