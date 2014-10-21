#' Creates a xpssFrame Object
#'
#' Creates numbers of new Variables
#'
#' \code{x} the input data should be of the format \emph{.csv, .sav or .xlsx}.
#' 
#' @param x character string with the name of the file.
#' @param as.data.table a logical indicating whether you want to return the input data in data.table format. 
#' Otherwise a data.frame returns. Default is FALSE.
#' @param \dots Arguments to pass on read.spss() from foreign.
#' @details The SPSS Variables are stored at the variable itself. A variable can have the following attributes: 
#' value.labels, defined.MIS, MIS, varname, variable.label
#' @author Andreas Wygrabek
#' @seealso \code{\link{read.spss}}
#' @examples  \dontrun{
#' daten <- xpssFrame("Testdata_1.sav")
#' }
#' 
#' @export
xpssFrame <- function(x, as.data.table = FALSE, ...){
    
    require("foreign", quietly = TRUE)
    require("data.table", quietly = TRUE)
    
    if (!(is.character(x))){
        stop("Input has to be a string")}

    data <- suppressWarnings(read.spss(x, ...))
    data <- as.data.table(data)
    classBackUp <- sapply(data,class)
    
    # ------ Matrix mit Missing-Positionen und Missing-Werten
    for(xxx in colnames(data)){
    missings <- eval(parse(text=paste("attributes(data)$missings$",xxx,"$value", sep = "")))
    
    # Es können Missings definiert sein, die nicht als Werte im Datensatz auftreten. Die definierten Werte werden zunächst als Attribut an die Variable gehangen
    eval(parse(text = paste("attr(data$",xxx,",'defined.MIS') <- missings",sep = ""))) 
    
    # Das Objekt missings wird nun dazu verwendet, eine Matrix mit den Positionen von Missing-Werten in den vorliegenden Daten zu erstellen
    
    if(length(missings > 0)){
    
    eval(parse(text = paste("POS <- which(is.element(data$",xxx,", missings))", sep = "")))
    eval(parse(text = paste("VAL <- data$",xxx,"[POS]", sep = "")))
    eval(parse(text = paste("missings <- cbind(POS, VAL)", sep = "")))

    
    eval(parse(text = paste("attr(data$",xxx,",'MIS') <- missings",sep = ""))) 
    }
    }
    
    # ------ Mapping originale SPSS-Werte und Labels

    for(i in names(data)){
        
        # --- Existiert das Attribut value label? Wenn nein, dann ist diese Variable numerisch. 
    if(eval(parse(text = paste("is.null(attributes(data)$label.table$",i,")", sep = "")))) {
        print(paste("No labels existing in", i, sep = " "))
    } else if (eval(parse(text = paste("length(attributes(data)$label.table$",i,")< ifelse(length(which(is.na(as.numeric(data$",i,"))))>0, length(unique(data$",i,"))-1,length(unique(data$",i,")))", sep = "")))){
        
        print(paste("Not every number is labelled in", i, sep = " "))
        
    } else {
        # --- Herausschreiben der value label und der numerischen Werte
    eval(parse(text = paste("valLab <- names(attributes(data)$label.table$",i,")", sep = "")))
    eval(parse(text = paste("numbers <- attributes(data)$label.table$",i, sep = "")))
    
    
    # 1.Schritt: NAs kopieren
    newVec <- numeric(length = nrow(data))
    eval(parse(text = paste("newVec[which(is.na(data$",i,"))] <- NA", sep = "")))
    
        # --- Zuordnung: Wo entspricht der Wert im Datensatz einem Label in den Attributen?
        #                An diese Stelle wird der in numbers gespeicherte Wert gesetzt 
    for(a in 1:length(valLab)){
        
        ########
        
        # Wo entsprechen die Daten dem ersten Label?
        eval(parse(text = paste("logVec <- data$",i," == valLab[",a,"]", sep = "")))
        # 2.Schritt: Originale Daten mit logVec 
        eval(parse(text = paste("newVec[logVec] <- numbers[",a,"]", sep = "")))
        
        ########

        }
    
    eval(parse(text = paste("data$",i," <- newVec",sep = "")))
    eval(parse(text = paste("attr(data$",i,", 'value.labels') <- numbers", sep = "")))
       }    
    }
    
    # Wenn spezifisches Attribut am Original-Import mitgeführt wurde, erstelle einen backup
    if(!is.null(attributes(data)$variable.labels))backup_varLabs <- attributes(data)$variable.labels
    if(!is.null(attributes(data)$names))backup_names <- attributes(data)$names
    if(!is.null(attributes(data)$class))backup_class <- attributes(data)$class
    if(!is.null(attributes(data)$rownames))backup_rownames <- attributes(data)$row.names
     
    # Schreibe den Backup an den Datensatz 
    if(exists("backup_class"))attr(data, "class") <- backup_class
    if(exists("backup_rownames")) attr(data, "rownames") <- backup_rownames
    
    # Schreiben des Variablennames als Attribut an die jeweilige Variable
    if(exists("backup_names")){
        for(i in colnames(data)){
            eval(parse(text = paste("attr(data$",i,", 'varname') <- backup_names[which('",i,"' == colnames(data))]", sep = "")))
        }
    }
    
    # Schreiben des Variablenlabel als Attribut an die jeweilige Variable
    attributes(backup_varLabs) <- NULL
      if(exists("backup_varLabs")){
          for(i in colnames(data)){
              eval(parse(text = paste("attr(data$",i,", 'variable.label') <- backup_varLabs[which('",i,"' == colnames(data))]", sep = "")))
          }
      }
    
    # Umwandeln der numerischen Missings in NA
    
     for(i in colnames(data)){
         
     eval(parse(text = paste("data$",i,"[attributes(data$",i,")$MIS[,1]] <- NA", sep = "")))
 
     }
     
    ##############################################################
    ################### Schreiben der Filterattribute ############
    ##############################################################
    
    # FILTER / TEMPORARY / SELECT IF / SPLIT FILE
    
    attr(data, "FILTER") <- FALSE
    attr(data, "TEMPORARY") <- FALSE
    attr(data, "SPLIT_FILE") <- FALSE
    attr(data, "DO_IF") <- FALSE
    attr(data, "SELECT_IF") <- FALSE
    attr(data, "WEIGHTS") <- "none"
    
    ##############################################################
    ##############################################################
    ##############################################################
    
    # Löschen des attributs label.table --> relikt aus dem foreign-Import
    attributes(data)$label.table <- NULL
    attributes(data)$missings <- NULL
    attributes(data)$variable.labels <- NULL
    
    # Rückgabe
    
    if(as.data.table == TRUE) {
        class(data) <- c("xpssFrame", "data.table")
        return(data)
    } else {
        data <- as.data.frame(data)
        class(data) <- c("xpssFrame", "data.frame")
        return(data)
    }
}



