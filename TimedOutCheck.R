# TimedOutCheck.R
# page = x

# Given a page pulled from the Scantron website, this function determines whether you got a timeout response
# 

#' @title Timed Out Check
#' @description Check whether you got what you wanted from the Scantron website using the
#'   page returned by an http call
#' @param page the content of a page pull from the Scantron website
#' @param messageLevel integer of length 1 indicating level of diagnostic
#'   messages to print
#' @return Logical.  If you got a timed out error, it returns TRUE.  Otherwise, FALSE.
#' @details This function checks for several different text strings that might
#'   appear in a timeout response.
TimedOutCheck = function(page, messageLevel = 0){
  
  if(messageLevel > 0){
    print("Checking to see if it worked.")
  }
  
  timedOutCheck = rep(F,4)
  timedOutCheck[1] = grepl(pattern = "<title>The wait operation timed out</title>", x = page, ignore.case = T)
  timedOutCheck[2] = grepl(pattern = "An unhandled exception occurred during the execution of the current web request", x = page, ignore.case = T)
  timedOutCheck[3] = grepl(pattern = "Please review the stack trace", x = page, ignore.case = T)
  timedOutCheck[4] = grepl(pattern = "A network-related or instance-specific error occurred while establishing a connection", x = page, ignore.case = T)
  
  timedOutCheck = any(timedOutCheck)
  
  return(timedOutCheck)
} #/TimedOutCheck function
