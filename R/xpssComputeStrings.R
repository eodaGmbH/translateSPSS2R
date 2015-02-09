
# 1.CHAR.INDEX --------------------------------------------------------------



#'  Locate position of first occurence of a pattern
#'
#'  Helper Function for xpssCompute. R Implementation of the SPSS \code{CHAR.INDEX} Function. 
#'
#' @usage computeChar_index (x,pattern = NULL, split = 0)
#' @param x character or character vector.
#' @param pattern pattern to look for its first occurence
#' @param split numeric. Number of parts pattern to divide to.  
#'
#' @return Numeric. Position of the first occurence of the \code{pattern}
#' @details \code{computeChar_index(x="Hello little user", pattern="user")} Returns a number indicating the character position of the first occurrence of the first letter of 'user' in 'Hello little user'. The optional third argument, \code{split}, is a number, which must be a positive integer used to divide 'user' into separate strings. The \code{split} value must be able to divide the \code{pattern} string without remainder.
#'
#' @author Bastian Wiessner
#' @seealso \code{\link{str_locate_all}}
#' @importFrom stringr str_length str_locate_all str_sub str_locate
#' @keywords internal
#' @examples 
#'
#' # Returns the position of the first letter of letter in 
#' # "there is no letter in word", "letter". Here 13
#' computeChar_index("there is no letter in word","letter")
#'
#' # Returns NA. The word "string" does not exist.
#' computeChar_index("there is no letter in word","string")
#' 
#' # x as a vector
#' x <- c("fruits are sweet", "fruits are sour", "fruits are salty")
#' computeChar_index(x,"sweet")
#' @keywords internal 
#' @export 


computeChar_index <- function(x,pattern = NULL, split = 0){
  if(!(is.numeric(split))){
    stop("argument split has to be numeric")
  }
  if(is.null(x) || is.null(pattern)){
    stop("argument x and pattern are empty.")
  }
  if(!(is.character(x)) || !(is.character(pattern))){
    stop("argument x and pattern has to be character")
  }
  out <- x
  # look after split is set
  if(split > 0) {
    # look if modulo division is > 1
    if((str_length(pattern) %% split) == 0) {
      # detect how long the patterns should be
      split_length <- str_length(pattern) / split
      # define startpos
      start_string <- 1
      # define end of string
      end_string <- split_length
      # create placeholder
      splitpattern <- vector()
      # loop over the amount of parts
      
      for(i in 1:split_length) {
        
        # create patterns
        splitpattern[[i]] <- str_sub(pattern, start = start_string, end = start_string+split-1)
      #  increment the start by length 
        start_string <- start_string+split
      }
      # create outputvar
      out <- unlist(str_locate_all(string=x,pattern=splitpattern))
      if(length(out) == 0){
        out <- 0
      } else {
        out <- min(out)
      }
      } else {
      stop("Rest has to be 0")
    }
  } else {
    # find first position of the pattern in string
    out <- unlist(str_locate_all(string=x,pattern=pattern))
    if(length(out) == 0){
      out <- 0
    } else {
      out <- min(out)
    }
  }
  return(out)
}

# 2. CHAR.LPAD ---------------------------------------------------------------


#' Expand strings on the left
#'
#'Helper Function for xpssCompute. R Implementation of the SPSS \code{CHAR.LPAD} Function. 
#'
#' @usage computeChar_lpad (x, length = NULL, fill = NULL)
#' @param x character or character vector.
#' @param length numeric. Number of characters x is to be filled on the left. 
#' @param fill optional. String which x should be filled with.
#'
#' @return String, left-padded by length \code{length}.   
#' 
#' @details The value of length represents the number of characters and must be a positive integer. If the optional argument \code{fill} is not specified, \code{x} is padded with blank spaces.
#' @author Bastian Wiessner
#' @seealso \code{\link{paste0}}
#' @importFrom stringr str_length 
#' @keywords internal
#' @examples 
#'
#' computeChar_lpad("My Friend",15)
#' computeChar_lpad("My Friend",15,"Hello ") # Muss noch angepasst werden, wenn Funktion fertig ist
#'
#' @export 

computeChar_lpad <- function(x, length = NULL, fill = NULL){
  if(is.null(length)){
    stop("argument length is empty.")
  }
  if(!(is.numeric(length))){
    stop("argument length has to be numeric.")
  }
  out <- x
  if(is.null(fill)){
    
    replicates <- length-str_length(x)
      for(i in 1:replicates){
      fill <- paste0(" ",fill)
    }
    out <- paste0(fill,out)
  } else{
    out <- ""
   for(i in 1:trunc(length/str_length(fill))){
      out <- paste0(fill,out)  
    }
   if(length>str_length(out)){
      remain <- length-str_length(out)
      fill <- substr(x,start = 1,stop = remain)
      out <- paste0(out,fill)
    }
  }
  return(out)
}

# 3. CHAR.LENGTH -------------------------------------------------------------


#' Length of a string in characters
#'
#'  Helper Function for xpssCompute. R Implementation of the SPSS \code{CHAR.LENGTH} Function. 
#'
#' @usage computeChar_length (x)
#' @param x character or character vector.
#'
#'
#'
#' @return Numeric. Returns the length of \code{x} in characters, with any trailing blanks removed.
#' @author Bastian Wiessner
#' @importFrom stringr str_length str_trim
#' @seealso \code{\link{str_length}}
#' @keywords internal
#' @examples 
#'
#' computeChar_length("            please remove trailing blanks")
#' 
#' computeChar_length("please remove trailing blanks")
#' 
#' x <- c("please","remove","trailing","blanks")
#' computeChar_length(x)
#' 
#' sum(computeChar_length(x))+3
#' @keywords internal
#' @export 


computeChar_length <- function(x){
  x <- str_trim(x)
 out <- str_length(x)
  return(out)
}

# 4. CHAR.MBLEN --------------------------------------------------------------


#' Byte per character or sign
#'
#'  Helper Function for xpssCompute. R Implementation of the SPSS \code{CHAR.MBLEN} Function. 
#'
#' @usage computeChar_mblen (x,pos = NULL)
#' @param x character vector, or a vector to be coerced to a character vector. Giving a factor is an error.
#' @param pos position of character or sign the number of bytes return to. 
#'
#'
#' @return Numeric. Returns the number of bytes in the character at position \code{pos} in \code{x}.
#' @details Important for Asian languages, where a character can fill more than one byte.
#' @author Bastian Wiessner
#' @seealso \code{\link{nchar}}
#' @keywords internal
#' @examples 
#'  
#'#Returns 1 cause the letter "R" fills one byte.
#' computeChar_mblen("'R' is great!",2)
#' 
#'#Returns 3 cause the asian sign "???" fills three bytes.
#' computeChar_mblen("???R???????????????????????????!",5)
#' 
#' string <- c("'R' is great!","???R???????????????????????????!","'R'??? ??????!","'R'????????????!")
#' computeChar_mblen(string,6)
#' @keywords internal
#' @export 


computeChar_mblen <- function(x,pos = NULL){
  if(is.null(pos)){
    stop("argument pos is empty.")
  }
  if(!(is.numeric(pos))){
    stop("argument pos has to be numeric")
  }
  out <- substr(x,start=pos,stop=pos)
  out <- nchar(x=out,type="bytes")  
return(out)
}

# 5. CHAR.RINDEX -------------------------------------------------------------


#' Locate position of last occurence of a pattern
#'
#'  Helper Function for xpssCompute. R Implementation of the SPSS \code{CHAR.RINDEX} Function. 
#'
#' @usage computeChar_rindex (x,pattern = NULL, split = 0)
#' @param x character or character vector.
#' @param pattern pattern to look for its last occurence
#' @param split numeric. Number of parts pattern to divide to.  
#'
#' @return Numeric. Position of the last occurence of the \code{pattern}
#' @details \code{computeChar_index(x="Hello little user", pattern="user")} Returns a number indicating the character position of the last occurrence of the first letter of 'user' in 'Hello little user'. The optional third argument, \code{split}, is a number, which must be a positive integer used to divide 'user' into separate strings. The \code{split} value must be able to divide the \code{pattern} string without remainder.
#' @author Bastian Wiessner
#' @seealso \code{\link{str_locate_all}}
#' @importFrom stringr str_length str_sub str_locate
#' @keywords internal
#' @examples 
#'
#' computeChar_rindex("At the end i?m looking for a good end","end")
#'
#' @export 


computeChar_rindex <- function(x,pattern = NULL, split = 0){
  if(!(is.numeric(split))){
    stop("argument split has to be numeric")
  }
  if(is.null(x) || is.null(pattern)){
    stop("argument x and pattern are empty.")
  }
  if(!(is.character(x)) || !(is.character(pattern))){
    stop("argument x and pattern has to be character")
  } 
  
  out <- x
  # look after split is set
  if(split > 0) {
    # look if modulo division is > 1
    if((str_length(pattern) %% split) == 0) {
      # detect how long the patterns should be
      split_length <- str_length(pattern) / split
      # define startpos
      start_string <- 1
      # define end of string
      end_string <- split_length
      # create placeholder
      splitpattern <- vector()
      # loop over the amount of parts
      for(i in 1:split) {
        # create i part 
        splitpattern[[i]] <- str_sub(pattern, start = start_string, end = end_string)
        # increment the start by length 
        start_string <- start_string+split_length
        # increment the end by length
        end_string <- end_string+split_length
      }
      # create outputvar
      out <- str_locate(string=x,pattern=splitpattern[[2]])[,1]
      # loop over the patterns
      for(i in 1:length(splitpattern)){
        # create a temporary output
        result_temp[[i]] <- str_locate_all(string=x,pattern=splitpattern[[i]])
        # loop over elements in the result
        for(j in 1:length(result_temp)){
          # check if the actual element is smaller than the existing
          if(length(result_temp[[i]][j][[1]][,1]) == 1 &&   result_temp[[i]][j][[1]][,1] > result[[j]]) {
            # if yes, override
            out[[j]] <- as.integer(result_temp[[i]][j][[1]][,1])            
          }
        }
      }

    } else {
      stop("Rest has to be 0")
    }
  } else {
    # find first position of the pattern in string
    result <-  str_locate_all(string=x,pattern=pattern)    
    for(j in 1:length(out)){
      if(length(result[[j]][nrow(result[[j]])]) == 0){
        out[[j]] <- NA
      } else {
        # look after last element
        out[[j]] <-  result[[j]][nrow(result[[j]])]
      }
    }    
    out <- unlist(out)
  }
  return(out)
}

# 6. CHAR.RPAD ---------------------------------------------------------------

#' Expand strings on the right
#'
#'  Helper Function for xpssCompute. R Implementation of the SPSS \code{CHAR.RPAD} Function. 
#'
#' @usage computeChar_rpad (x, length = NULL, fill = NULL)
#' @param x character or character vector.
#' @param length numeric. Number of characters x is to be filled on the right. 
#' @param fill optional. String which x should be filled with.
#'
#' @return String, right-padded by length \code{length}.   
#' 
#' @details The value of length represents the number of characters and must be a positive integer. If the optional argument \code{fill} is not specified, \code{x} is padded with blank spaces.
#' @author Bastian Wiessner
#' @seealso \code{\link{paste0}}
#' @importFrom stringr str_length 
#' @keywords internal
#' @examples 
#'
#' computeChar_rpad("My Friend",15)
#' computeChar_rpad("My Friend",15,"Hello ") # Muss noch angepasst werden, wenn Funktion fertig ist
#'
#' @export 


computeChar_rpad <- function(x, length = NULL, fill = NULL){
  if(is.null(length)){
    stop("argument length is empty.")
  }
  if(!(is.numeric(length))){
    stop("argument length has to be numeric")
  }
  
  out <- x
  if(is.null(fill)){
    replicates <- length-str_length(x)
    for(i in 1:replicates){
      fill <- paste0(" ",fill)
    }
    paste0(fill,out)
  } else {
    for(i in 1:length(x)){
      while(length>str_length(paste0(out[[i]],fill))){
        out[[i]] <- paste0(out[[i]],fill)  
      }
    }
    if(length>str_length(out)){
      remain <- length-str_length(out)
      fill <- substr(fill,start = 1,stop = remain)
      out <- paste0(out,fill)
    }
  }  
  return(out)
}

# 7. CONCAT ------------------------------------------------------------------


#' computeConcat (x, sep = "")
#'
#'  Helper Function for xpssCompute. R Implementation of the SPSS \code{CONCAT} Function. 
#'
#' @usage computeConcat (x, sep = "")
#' @param x character or character vector.
#' @param sep atomic character element which splits the input data. Default is "".
#'
#'
#' @return String. Returns a string that is the concatenation of all its arguments.
#' @author Bastian Wiessner
#' @seealso \code{\link{paste0}}
#' @keywords internal
#' @examples 
#' 
#' x <- c("this","is","a","vector")
#' computeConcat(x)
#'
#' @export 

computeConcat <- function(x, sep = ""){
  
  out <- character()
  for(i in 1:length(x)){
    out <- paste(out,x[[i]],sep="")
    if(i < length(x)) {
      out <- paste(out," ")  
    }
  }  
  return(out)
}

# 8. LENGTH ------------------------------------------------------------------


#' Number of bytes in a string
#'
#'  Helper Function for xpssCompute. R Implementation of the SPSS \code{LENGTH} Function. 
#'
#' @usage computeLength (x)
#' @param x character or character vector.
#'
#'
#'
#' @return Numeric. Returns the length of \code{x} in bytes, including all trailing blanks. 
#'
#' @author Bastian Wiessner
#' @seealso \code{\link{nchar}}
#' @keywords internal
#' @examples 
#'
#' computeLength("trailing blanks matter")
#' computeLength("trailing blanks matter          ")
#' computeLength("        trailing blanks matter")
#' 
#' @export 


computeLength <- function(x){
  out <- nchar(x,type="bytes")
  return(out)
}

# 9. LOWER -------------------------------------------------------------------


#' Convert upper-case letters to lower-case 
#'
#'  Helper Function for xpssCompute. R Implementation of the SPSS \code{LOWER} Function. 
#'
#' @usage computeLower (x)
#' @param x a character vector, or an object that can be coerced to character by as.character.
#'
#'
#'
#' @return String. Returns unput with uppercase letters changed to lowercase. The argument can be a string variable or a value. 
#' @author Bastian Wiessner
#' @seealso \code{\link{tolower}}
#' @keywords internal
#' @examples 
#'
#' computeLower("MAKE ME small PLEASE")
#'
#' computeLower("From BIG 2 small")
#' @export 


computeLower <- function(x){
  out <- tolower(x)
  return(out)
}

# 10. LTRIM -------------------------------------------------------------------


#' Trims string on left side
#'
#'  Helper Function for xpssCompute. R Implementation of the SPSS \code{LTRIM} Function. 
#'
#' @usage computeLtrim (x,trim = NULL)
#' @param x input character vector
#' @param trim single quoted character or an expression that yields a single character. 
#'
#'
#' @return String. Returns the input string removed by \code{trim} on the left. If trim is not specified, leading blanks are removed. 
#' @author Bastian Wiessner
#' @importFrom stringr str_locate str_trim
#' @seealso \code{\link{str_trim}}
#' @keywords internal
#' @examples 
#'
#' \dontrun{beispiel <- beispiel()}
#'
#' @export 

computeLtrim <- function(x,trim = NULL){
  if(!(is.null(trim))){
    if(str_length(trim)>1){
      stop("trim argument is limited to one value")
    }  
  }
  if(trim == " ") {
    out <- str_trim(x,side="left")  
  } else {
    out <- character()
    x <- str_split(x,pattern = " ")
    for(i in 1:length(x[[1]])){
      if(length(str_locate_all(x[[1]][[i]],trim)[[1]])>0){
        if(min(str_locate_all(x[[1]][[i]],trim)[[1]][,1]) == 1){
          temp <- substring(x[[1]][[i]],2,nchar(x[[1]][[i]]))     
          out <- paste(out,temp)
        } else {
          out <- paste(out,x[[1]][[i]])
        }
      } else {
        out <- paste(out,x[[1]][[i]])
      }
    }
  }
  return(out)
}

# 11. REPLACE -----------------------------------------------------------------

#' Replace matched pattern in a string
#'
#'  Helper Function for xpssCompute. R Implementation of the SPSS \code{REPLACE} Function. 
#'
#' @usage computeReplace (x,pattern=NULL,match=NULL, count = NULL)
#' @param x input vector 
#' @param pattern pattern to look for
#' @param match string tp replace with
#' @param count number of occurencees of pattern to replace
#' @return String. Returns a character vector
#' @author Bastian Wiessner
#' @seealso \code{\link{str_replace}}\code{\link{str_replace_all}}\code{\link{grep}}
#' @importFrom stringr str_replace_all str_locate_all str_sub<-
#' @keywords internal
#' @examples 
#'
#' computeReplace("Makfts sftnsft","ft","e")
#'
#' @export 

computeReplace <- function(x,pattern=NULL,match=NULL, count = NULL){
  if(is.null(match) || is.null(pattern))
  {
    stop("pattern and match cant be null")
  }
  if(is.null(count)){
   out <-  str_replace_all(string=x,pattern=pattern,replacement=match)      
  }else {
    position <- str_locate_all(string=x,pattern)
    for(i in 1:count){
      # replace the amount of of count arguments with match
      str_sub(string=x,start=position[[1]][,1][[i]],end=position[[1]][,2][[i]]) <- match
    }
    out <- x
  }
  return(out)
}

# 12. RTRIM -------------------------------------------------------------------


#' Trims string on right side
#'
#'  Helper Function for xpssCompute. R Implementation of the SPSS \code{RTRIM} Function. 
#'
#' @usage computeRtrim (x,trim = NULL)
#' @param x input character vector
#' @param trim single quoted character or an expression that yields a single character. 
#'
#'
#' @return String. Returns the input string removed by \code{trim} on the right. If trim is not specified, trailing blanks are removed. 
#' @author Bastian Wiessner
#' @seealso \code{\link{str_trim}}
#' @importFrom stringr str_trim str_locate
#' @keywords internal
#' @examples 
#'
#' \dontrun{beispiel <- beispiel()}
#'
#' @export 

computeRtrim <- function(x,trim = NULL){
  if(!(is.null(trim))){
    if(str_length(trim)>1){
      stop("trim argument is limited to one value")
    }  
  }
  if(trim == " ") {
    out <- str_trim(x,side="right")  
  } else {
    out <- character()
    x <- str_split(x,pattern = " ")
    for(i in 1:length(x[[1]])){
      if(length(str_locate_all(x[[1]][[i]],trim)[[1]])>0){
        if(max(str_locate_all(x[[1]][[i]],trim)[[1]][,1]) == str_length(x[[1]][[i]])){
          temp <- substring(x[[1]][[i]],1,nchar(x[[1]][[i]])-1)     
          out <- paste(out,temp)
        } else {
          out <- paste(out,x[[1]][[i]])
        }
      } else {
        out <- paste(out,x[[1]][[i]])
      }
    }
  }
  return(out)
}


# 13. STRUNC ------------------------------------------------------------------


#' Truncating strings
#'
#'  Helper Function for xpssCompute. R Implementation of the SPSS \code{STRUNC} Function. 
#'
#' @usage computeStrunc (x, length = NULL)
#' @param x input character vector.
#' @param length length x is truncated to
#'
#'
#' @return String. Returns \code{x} truncated to \code{length} (in bytes) and trimmed of remaining blanks. 
#' @author Bastian Wiessner
#' @importFrom stringr str_sub
#' @seealso \code{\link{str_sub}}
#' @keywords internal
#' @examples 
#'
#' computeStrunc("Hello    ",7)
#'
#' @export 


computeStrunc <- function(x, length = NULL){
  if(is.null(length)){
    stop("length cant be null")
  } else{
    if(!(is.numeric(length))){
      stop("length has to be numeric")
    }
  }  
  out <- str_sub(x,start=1,end=length)
  out <- str_trim(string = out,side = "right")
  return(out)
}

# 14. UPCASE ------------------------------------------------------------------


#' Convert lower-case letters to upper-case 
#'
#'  Helper Function for xpssCompute. R Implementation of the SPSS \code{UPCASE} Function. 
#'
#' @usage computeUpcase (x)
#' @param x a character vector, or an object that can be coerced to character by as.character.
#'
#'
#'
#' @return String. Returns unput with lowercase letters changed to uppercase. The argument can be a string variable or a value. 
#' @author Bastian Wiessner
#' @seealso \code{\link{toupper}}
#' @keywords internal
#' @examples 
#'
#' computeUpcase("make it big")
#' 
#' small <- c("make","all","big")
#' computeUpcase(small)
#'
#' @export 


computeUpcase <- function(x){
  out <- toupper(x)
  return(out)
}



