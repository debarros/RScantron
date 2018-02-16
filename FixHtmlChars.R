# FixHtmlChars.R

FixHtmlChars = function(x, messageLevel = 0){
  if(messageLevel > 0){
    print("Fixing weird html characters")
  }
  x = gsub("&gt;", ">", x)  # switch from html code for > to just the symbol
  x = gsub("&amp;", "&", x) # switch from html code for & to just the symbol
  return(x)
} # /function
