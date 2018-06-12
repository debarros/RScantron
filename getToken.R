#getToken.R

# This function takes the following arguments:
#   x - the initial login page
# It finds and return the post form value "__RequestVerificationToken"
# This is necessary (I think) for maintaining a consistent identity during the login process


#' @title Get Token
#' @description get the request verification token from the initial login page
#' @param x the initial login page
#' @param messageLevel integer of length 1 indicating level of diagnostic
#'   messages to print
#' @return character string containing the login token
#' @details This function extracts the request verification token from the
#'   content of the initial login page.  It is necessary for maintaining a
#'   consistent identity during the login process.
getToken = function(x, messageLevel = 0){
  if(messageLevel > 0){
    print("getting the request verification token")
  }
  TokenInputStart = regexpr(pattern = "__RequestVerificationToken", text = x)[1] #Find where the input tag starts
  ValueLocations = gregexpr(pattern = 'value=', text = x)[[1]]                   #Find where the "value"s are
  TokenStart = ValueLocations[ValueLocations > TokenInputStart][1] + 7           #Find the end of the first "value" after the start of the tag
  QuoteLocations = gregexpr(pattern = '\\"', text = x)[[1]]                      #Find where the quotation marks are
  TokenEnd = QuoteLocations[QuoteLocations > TokenStart][1]-1                    #Find the quote at the end of the token
  return(substr(x, TokenStart, TokenEnd))                                        #Return the token
} # /getToken function
