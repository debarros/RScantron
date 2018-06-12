# FixHtmlChars.R

#' @title Fix HTML Characters
#' @description Replace HTML codes with the regular text characters
#' @param x the content of a webpage
#' @param messageLevel integer of length 1 indicating level of diagnostic messages to print
#' @return character of length 1 containing the same content as \code{x}, but with weird characters fixed
#' @details This function is expecially important when trying to find character strings in the content of a webpage
FixHtmlChars = function(x, messageLevel = 0){
  if(messageLevel > 0){
    print("Fixing weird html characters")
  }
  x = gsub("&gt;", ">", x)  # switch from html code for > to just the symbol
  x = gsub("&amp;", "&", x) # switch from html code for & to just the symbol
  return(x)
} # /FixHtmlChars function
