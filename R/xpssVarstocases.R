#' Transforms variables to cases
#'
#' Creates a transfromed xpssFrame.
#'
#' @usage xpssVarsToCases(x, from, idVar = NULL, indexVar = NULL, nullArg = "keep", 
#' countVar = NULL, varLabels = list(id = NULL, index = NULL, count = NULL))
#' @param x as a (non-empty) data.frame, data.table object or input data of class "xpssFrame". 
#' @param from variable that opens the span.
#' @param idVar determines whether an id-variable should be created.
#' @param indexVar determines whether an index-variable should be created.
#' @param nullArg Can be either "keep" or "drop".
#' @param countVar determines whether a counter should be created?
#' @param varLabels determines whether labels for id-, index- and count variables are set.
#' @return Returns the transformed xpssFrame.
#' @author Andreas Wygrabek
#' @importFrom plyr ldply llply
#' @importFrom tidyr gather
#' @examples 
#' # load data
#' data(fromXPSS)
#'
#' # write V7_1 and V7_2 in newvar
#' xpssVarsToCases(fromXPSS, from = list(c("newVar", "V7_1, V7_2")), 
#' idVar = "myID", indexVar = "myIndex", nullArg = "drop", countVar = "Counter")
#' @export
xpssVarsToCases <- function(x, from, idVar = NULL, indexVar = NULL, nullArg = "keep", countVar = NULL, varLabels = list(id = NULL, 
                                                                                                                      index = NULL,
                                                                                                                        count = NULL)){
options(warn = -1)

####################################################################
####################### Meta - Checks ##############################
####################################################################

functiontype <- "DM"
x <- applyMetaCheck(x)

####################################################################
####################################################################
####################################################################


if(length(from) < 1){
    stop("Argument from is required")
}

testLengthFrom <- sapply(from, function(x){
    length(x)
})

if(sum(testLengthFrom)/length(from) != 2){
    stop("Each element of from has to be length 2")
}  


if(!is.null(varLabels[["id"]]) & is.null(idVar)){
    stop("Create an idVar first, before you label it")
}

if(!is.null(varLabels[["index"]]) & is.null(indexVar)){
    stop("Create an indexVar first, before you label it")
}

if(!is.null(varLabels[["count"]]) & is.null(countVar)){
    stop("Create a countVar first, before you label it")
}

# Getting variables to switch
vars <- llply(.data = from, 
              function(x){
                  x[[2]]
              })

# ID-Variable for sorting the cases as SPSS do
x[,"idVar"] <- 1:nrow(x)

# Getting variable names
varnames <- llply(.data = from, 
                  function(x){
                      x[[1]]
                  })

# First run to receive all variables
eval(parse(text = paste("ini <- gather(x,'key','make'",",",unlist(vars)[1],")", sep = "")))
#  Remove make
ini <- ini[,-which(colnames(ini)=="key" | colnames(ini)=="make")]

# Get all values
eval(parse(text = paste("temp",1:length(from)," <- gather(x,'key','make'",",",unlist(vars),")[,'make']", sep = "")))
# Bind objects with values to matrix
mat <- do.call("cbind", as.list(parse(text = paste0("temp",1:length(vars)))))

# Name the variables
colnames(mat) <- varnames

out <- cbind(ini, mat)

# Use of nullArg-Argument
if(nullArg == "drop"){
    if(length(colnames(mat)) > 1){
    ind <- rowSums(mat, na.rm = TRUE)
    ind <- which(!is.na(ind))} else {
        ind <- which(!is.na(mat[,1]))
    }
} else if(nullArg == "keep") {
    ind <- rep(TRUE,nrow(out))
}

# Use of countArg
if(!is.null(countVar) & nullArg == "keep"){
    
    counts <- ldply(split(out, out[,"idVar"]),
                    function(x){
                        nrow(x)
                    })
    
     out[,countVar] <- apply(out,1,function(x){
         
         POS <- which(as.integer(x["idVar"]) == 1:nrow(counts)) 
         counts[POS,2]
     })
} else if(!is.null(countVar) & nullArg == "drop") {
    
    counts <- ldply(split(out[ind,], out[ind,"idVar"]),
                    function(x){
                        nrow(x)
                    })
    
    out[,countVar] <- apply(out,1,function(x){
        
        POS <- which(as.integer(x["idVar"]) == 1:nrow(counts)) 
        counts[POS,2]
    })
}

# Use of idVar-Argument
if(!is.null(idVar)){
    out <- out[,c("idVar", colnames(out)[-length(colnames(ini))])]
    out <- xpssSortCases(out,"idVar","A")
    colnames(out)[colnames(out) == "idVar"] <- idVar 
} else {
    out <- xpssSortCases(out,"idVar","A")
    out[,which(colnames(out) == "idVar")] <- NULL
}

# Use of indexVar-Argument
if(!is.null(indexVar)){
    out[,indexVar] <- rep(1:(nrow(out)/nrow(x)),length(unique(out[,1])))
}

# Backup of attributes
attrDF <- attributes(out)
attrVars <- sapply(out, function(x){
    attributes(x)
})
orderRN <- rownames(out[(rownames(out) %in% ind) | ind == TRUE,])

out <- out[(rownames(out) %in% ind) | ind == TRUE,]

# Apply attribute backup
attrDF$row.names <- orderRN
attributes(out) <- attrDF
eval(parse(text = paste("attributes(out[,",1:length(colnames(out)),"]) <- attrVars[[",1:length(colnames(out)),"]]", sep = "")))
rownames(out) <- orderRN

# Global Backup Attributes fill into existent 
attr_backup <- attributesBackup(x)

pos <- which(!names(attr_backup$global) %in% names(attributes(out)))

for(i in 1:length(names(attr_backup$global)[pos])) {
  attr(out,names(attr_backup$global)[pos][i]) <- attr_backup$global[pos][[i]]
}

class(out) <- c("xpssFrame", "data.frame")

# Set labels 
if(!is.null(idVar) & !is.null(varLabels[["id"]])){
    out <- xpssVariableLabels(out, variables = c(idVar), labels = varLabels[[1]])}

if(!is.null(indexVar) & !is.null(varLabels[["index"]])){
    out <- xpssVariableLabels(out, variables = c(indexVar), labels = varLabels[[2]])}

if(!is.null(countVar) & !is.null(varLabels[["count"]])){
    out <- xpssVariableLabels(out, variables = c(countVar), labels = varLabels[[3]])}

options(warn = 0)

return(out)
}
