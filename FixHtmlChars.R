# FixHtmlChars.R

FixHtmlChars = function(x){
  x = gsub("&gt;", ">", x)  # switch from html code for > to just the symbol
  x = gsub("&amp;", "&", x) # switch from html code for & to just the symbol
  return(x)
} # /function