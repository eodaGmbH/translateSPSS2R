#' @export

xpssDate11 <- function(){
  
  (today <- Sys.Date())
  today <- format(today, "%d-%b-%Y")  # with month as a word
  today <- toupper(today)
  
  return(today)
}
