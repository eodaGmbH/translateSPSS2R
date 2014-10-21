#' Recodes variables
#'
#' xpssRecode recodes atomic or vectors of the format numeric, character or factor under the terms of recode specifications.
#'
#' The input of the recoding is a character string with the recoding procedure seperated with a semicolon and a else statement. 
#' \describe{
#'    \item{\code{single data transformation}:}{ \code{rec = "1=99; else = test"}}
#'    \item{\code{For a numeric vector transformation}:}{\code{rec = "c(1,2,3)=1; else =11"}}
#'    \item{\code{For a character vector transformation}:}{\code{rec = "c('A','B')='AB'; c('C','D')='CD'; else = 'ZZ'"}}
#'    \item{\code{For a range of values}:}{\code{rec = "lo:10 = 1; 11: 22= 2; 23:hi= 3; else = 'copy'"}.}
#'  }
#'  \strong{NOTE:} \code{lo} and  \code{hi} are special values and determine the lowest and highest value of a numeric value. \cr
#' The \code{":"}-Operator differs in this context from the sequence operator. In xpssRecode it specifies the range from A to B. \cr 
#' f.e. 1:10 defines the valuerange for a variable, all values which are within this range get recoded. \cr \cr 
#'      
#' The \code{else} statement indicates the handling of the values which arent selected by the recoding statement, respectively this statement matches all  unspecified values, including missing values. \cr 
#' System default, if no else statement is given, is \code{else='copy'}. 
#' 
#' \tabular{rlll}{
#' 
#' \tab \code{else='copy'} \tab overwrites all unmatched values with the original value \cr 
#' \tab \code{else='NA'} \tab overwrites all unmatched values in the new dataset with \code{NA}.\cr 
#' \tab \code{else='Other'} \tab overwrites all unmatched values with Other, \strong{only} possible for character values. \cr 
#' \tab \code{else=99} \tab overwrites all unmatched values with 99, \strong{only} possible for numeric values.} \cr \cr 
#'
#' \code{varout} determines if a new variable with the recoded values should appended at the end of the dataset.
#'
#' @param x a (non-empty) data.frame, data.table object or input data of class "xpssFrame". 
#' @param varin Variables to recode
#' @param rec Character string with recoding specifications: for more informations see details.
#' @param varout Where to store the recoded Variables
#' @return The function returns the data frame x with the recoded variables
#' @author Andreas Wygrabek
#' @export 
#' @examples \dontrun{
#' data(fromXPSS)
#' temp <- xpssRecode(fromXPSS,
#'            varin ="V1", 
#'            rec="'Audi' = 'Porsche'; else= copy",
#'            varout = NULL)
#'          
#' temp <- xpssRecode(fromXPSS, 
#'                    varin = c("V5","V7_2"),
#'                    rec = "lo:50 =1; 51:200=2; 201:hi=3; else = COPY",
#'                    varout =c("V5_new","V7_new"))
#'                    
#' temp <- xpssRecode(fromXPSS, 
#'                    varin = c("V6_kl3","V7_2"),
#'                    rec = "sysmis = 99; else= 3",
#'                    varout =c("V5_new","V7_new"))
#'          
#' }
xpssRecode <- function(x, varin = NULL, rec = NULL, varout = NULL){

# Die Eingabe von varin und varout erfolgt als Character
# logvec wird als String übergeben

require("car")
require("stringr")


  class(x) <- c("xpssFrame","data.frame","DM")  


####################################################################
####################### Meta - Checks ##############################
####################################################################
x <- applyMetaCheck(x)
####################################################################
####################################################################
####################################################################

### Checks
# Wenn varin UND varout vorhanden sind, müssen beide vektoren gleich lang sein. 
# Wenn varout nicht vorhanden ist, sind die Ausgabevariablen gleich den Eingabevariablen

if(length(varin) != length(varout) & length(varout) != 0){
    stop("varin and varout are not of the same length")
} else if(length(varout) == 0){
    varout <- varin
}

attBack <- attributesBackup(x)


####
recBack <- rec
rec <- tolower(rec)
naDet <- str_detect(rec, "na")
rec <- gsub("na", "NA", rec)
express  <- str_split(str_replace_all(rec,pattern=" ",""),"=")


if("sysmis" %in% express[[1]]) {
  for(i in 1:length(varin)) {
    x <- xpssValue(x,variables=varin[[i]])  
  }
}


## Abfangen von SPSS Statements
recBack <- gsub(pattern="lowest",replacement="lo",x=recBack)
recBack <- gsub(pattern="highest",replacement="hi",x=recBack)
recBack <- gsub(pattern="missing",replacement="NA",x=recBack)
recBack <- gsub(pattern="sysmis",replacement="NA",x=recBack)


unlistRec <- unlist(str_split(rec, pattern = ";"))
# Es wird nur noch else gesucht. Denkbar wäre auch die Suche nach "copy". Ein Kommando wie "Audi" = COPY
# hat in SPSS jedoch effektiv keine Wirkung auf die Ausgabevariable. 
posElse <- which(str_detect(unlistRec, pattern = "else"))

charRec <- unlist(str_split(rec, pattern = ";"))
charRec <- paste(charRec[-posElse], collapse = ";")

###################################################################
# Globaler TEST 
# Prüfen auf subcommands copy/else 
COPY <- grepl("copy", rec)
ELSE <- grepl("else", rec)
ELSECOPY <- grepl("else[[:blank:]]*=[[:blank:]]*copy", rec)
###################################################################
inlist <- list() # Arbeitsdaten der globalen Schleife
outlist <- list()

##### 
##### Erstelle 2 Listen mit den Variablen die Ein- bzw. ausgehen. 
for(i in 1:length(varin)){
    
    inlist[[i]] <- x[,varin[i]]
    if(varout[i] %in% colnames(x)){
        outlist[[i]] <- x[,varout[i]]
    } else {
        if(is.numeric(x[,varin[i]])){
        outlist[[i]] <- numeric(length = nrow(x))
        } else {
        outlist[[i]] <- character(length = nrow(x))
        }
    }
} 


# Wenn varin ein character --> umwandeln in lower cases und entfernen von blanks("   ") im String
for(i in 1:length(inlist)){
    if(is.character(inlist[[i]])){
        inlist[[i]] <- gsub("[[:blank:]]*", "", inlist[[i]]) 
    }
}

# Hier wird die globale Schleife eingeleitet: 
for(i in 1:length(varin)){
     
    Varin <- inlist[[i]]
    Varout <- outlist[[i]]    

    ##############################################
    ##############################################
    ##############################################
    # Wenn nicht COPY aber ELSE
    if(COPY == FALSE & ELSE  == TRUE & is.numeric(Varin)){RET <- car::Recode(var = Varin, recodes = recBack)}                                       
    if(COPY == FALSE & ELSE  == TRUE & is.character(Varin)){
        recBackCut <- unlist(str_split(recBack, pattern = ";"))
        charRecElse <- recBackCut[posElse]
        charRecElse <- str_extract(charRecElse, "'.+'") 
        
        charRecElse <- paste("else = ", charRecElse)
        rec <- paste(recBackCut[-posElse], ";", charRecElse)
        RET <- car::Recode(var = Varin, recodes = rec)} 
    
    # Wenn ELSE = COPY vorhanden:
    if(ELSECOPY == TRUE)  {
        
        # Bilden von tempRec, die den copy-Befehl ausschlie?t
        tempRec <- unlist(str_split(recBack, pattern = ";"))
        pos <- which(str_detect(tempRec, pattern = "copy"))
        tempRec <- paste(tempRec[-pos], collapse = ";") 
        
        if(is.numeric(Varin)){
            tempRec <- paste(tempRec, "; else =", max(Varin, na.rm = TRUE)+1, sep = "")
        
            RET <- car::Recode(var = Varin, recodes = tempRec)
        
            RET[RET == max(Varin, na.rm = TRUE)+1] <- Varin[RET == max(Varin, na.rm = TRUE)+1]
        } else {
          
          charRec <- unlist(str_split(tempRec, pattern = ";"))
          charRec <- paste(charRec[-posElse], collapse = ";")
            tempRec <- paste(charRec, "; else =", "'AHLEWORSCHT'", sep = "")
            
            RET <- car::Recode(var = Varin, recodes = tempRec)

            RET[RET == "AHLEWORSCHT"] <- Varin[RET == "AHLEWORSCHT"]
        }
    }
    
    # Wenn kein ELSE und kein COPY
    if(ELSE == FALSE & COPY == FALSE){
        ###
        RET_st <- car::Recode(var = Varin, recodes = recBack)
        ###
        tempRec <- paste(recBack, "; else = NA", sep = "")
        RET <- car::Recode(var = Varin, recodes = tempRec)
        RET[which(is.na(RET)&!is.na(RET_st))] <- Varout[which(is.na(RET)&!is.na(RET_st))]
    }
    
    # "Wenn COPY vorhanden und ELSE nicht vorhanden" ODER "COPY vorhanden und ELSE vorhanden" 
    if(COPY == TRUE & ELSECOPY == FALSE) {
        ################## Eine tempor?re Recode-Variable wird gebildet. 
        ################## Diese enth?lt alle Recodes ausgenommen der copy-Anweisung
        ###### Schritt1 : Entfernen der COPY Anweisung
        SPLIT <- unlist(str_split(rec,pattern = ";"))
        WHERE <- which(str_detect(SPLIT, pattern = "copy"))  
        SPLIT <- SPLIT[-WHERE]  
        ###### Schritt2 : Durchf?hren des RECODES ohne COPY
        ###### Filter: Gibt es ein ELSE oder nicht 
        newRec <- paste(SPLIT, collapse = ";")
        if(ELSE == FALSE){ 
            tempRec <- paste(newRec, "; else = NA", sep = "")
            tempRET <- car::Recode(var = Varin, recodes = tempRec)
        } else {  
            tempRec <- paste(newRec, sep = "")
            tempRET <- car::Recode(var = Varin, recodes = tempRec)  
            tempRET[which(is.na(tempRET))] <- Varout[which(is.na(tempRET))]
        }   
        ################## Verarbeiten von copy
        ###### Schritt3 : Erstelle Vektoren mit Recode-Anweisungen. 
        SPLIT <- unlist(str_split(rec ,pattern = ";"))
        IN <- character(length = length(SPLIT))
        OUT <- character(length = length(SPLIT))
        for (ii in 1:length(SPLIT)){    
            IN[ii] <- unlist(str_split(SPLIT[ii], "="))[1]
            OUT[ii] <- unlist(str_split(SPLIT[ii], "="))[2]
        }
        ###### Schritt4 : Untersuchen, welche Daten an copy ?bergeben werden 
        #WERT <- str_detect(IN[which(!is.na(str_extract(OUT, pattern = "copy")))], pattern = "[:digit]")
        DOPPEL <- str_detect(IN[which(!is.na(str_extract(OUT, pattern = "copy")))], pattern = ":")
        KOMMA <- str_detect(IN[which(!is.na(str_extract(OUT, pattern = "copy")))], pattern = ",")  
        ####
        #### Recode, wenn es sich um einen Scope handelt
        ####  
        if(DOPPEL == TRUE) {DOPPEL_st <- unlist(str_split(IN[which(!is.na(str_extract(OUT, pattern = "copy")))], pattern = ":"))[1]
                            DOPPEL_nd <- unlist(str_split(IN[which(!is.na(str_extract(OUT, pattern = "copy")))], pattern = ":"))[2]}
        # Wert vor Doppelpunkt
        if(exists("DOPPEL_st")) {DOPPEL_st <- as.numeric(DOPPEL_st)}
        # Wert nach Doppelpunkt
        if(exists("DOPPEL_nd")) {DOPPEL_nd <- as.numeric(DOPPEL_nd)}
        
        # Abfrage nach Schl?sselworten lo/hi 
        
        if(DOPPEL == TRUE) {loTest <- str_detect(IN[which(!is.na(str_extract(OUT, pattern = "copy")))],"lo")}
        if(DOPPEL == TRUE) {hiTest <- str_detect(IN[which(!is.na(str_extract(OUT, pattern = "copy")))],"hi")}
        ###### Schritt5 : Werte indizieren 
        if(DOPPEL == TRUE & !exists("loTest") & !exists("hiTest")) {tempvarin <- ifelse(Varin >= DOPPEL_st & Varin <= DOPPEL_nd, Varin, NA)
                                                                    RET <- ifelse(!(is.na(tempvarin)), tempvarin, tempRET)}
        
        if(DOPPEL == TRUE & exists("loTest")) {tempvarin <- ifelse(Varin >= min(Varin, na.rm = TRUE) & Varin <= DOPPEL_nd, Varin, NA)
                                               RET <- ifelse(!(is.na(tempvarin)), tempvarin, tempRET)}
        
        if(DOPPEL == TRUE & exists("hiTest")) {tempvarin <- ifelse(Varin >= DOPPEL_st & Varin <= max(Varin, na.rm = TRUE), Varin, NA)
                                               RET <- ifelse(!(is.na(tempvarin)), tempvarin, tempRET)}
        ####
        #### Recode, wenn es sich um einen Vektor handelt
        ####    
        if(KOMMA == TRUE) {RET <- ifelse(Varin %in% eval(parse(text = IN[which(!is.na(str_extract(OUT, pattern = "copy")))])), Varin, tempRET)}
        ####
        #### Recode, wenn es sich um einen atomaren Vektor handelt
        ####  
        if(DOPPEL == FALSE & KOMMA == FALSE)  {AV <- as.numeric(IN[which(!is.na(str_extract(OUT, pattern = "copy")))])
                                               RET <- ifelse(Varin %in% AV, Varin, tempRET)}
        ################################################
        ############# END OF ELSE AND COPY #############                                   
        ################################################
    }
    
    outlist[[i]] <- RET
   
}
    for(i in 1:length(varout)){
           x[,varout[i]] <- numeric(length = nrow(x))
           x[,varout[i]] <- outlist[[i]]
    }

x <- applyAttributes(x=x,attributesToApply=attBack)


# zurückschreiben der attribute, wenn kein else gesetzt ist
if("sysmis" %in% express[[1]]) {
  for(i in 1:length(varin)) {
    if(length(attributes(eval(parse(text=paste("x$",varin[[i]]))))$MIS)>0) {
      pos <- attributes(eval(parse(text=paste("x$",varin[[i]]))))$MIS[,1]
      x[,varin[i]][pos] <- NA
    }
  }
}

### DeMerge
## Filter is set & Function is a Datamanagement Function
x <- applyAttributeDemerge(x)

return(x)
}



