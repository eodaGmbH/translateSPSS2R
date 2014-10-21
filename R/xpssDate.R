#' @export

xpssDate <- function(){

  (today <- Sys.Date())
  today <- format(today, "%d-%b-%y")  # with month as a word
  today <- toupper(today)
  
  return(today)
}
