# BadReturnCheck.R
# page = x

# Given a page pulled from the Scantron website, this function determines whether you are logged out
# 

#' @title Bad Return Check
#' @description Check whether you are logged into the Scantron website using the
#'   page returned by an http call
#' @param page the content of a page pull from the Scantron website
#' @param messageLevel integer of length 1 indicating level of diagnostic
#'   messages to print
#' @return Logical.  If you are logged out, it returns TRUE.  Otherwise, FALSE.
#' @details This function checks for several different text strings that might
#'   appear an http call is made while not logged in.
BadReturnCheck = function(page, messageLevel = 0){
  
  if(messageLevel > 0){
    print("Checking to see if you are logged in.")
  }
  
  badReturnCheck = rep(F,4)
  badReturnCheck[1] = grepl(pattern = "Admin Site, Enter Your Username/Password", x = page, ignore.case = T)
  badReturnCheck[2] = grepl(pattern = "Admin Site, Enter Your Site ID", x = page, ignore.case = T)
  badReturnCheck[3] = grepl(pattern = "header_loggedout_container", x = page, ignore.case = T)
  badReturnCheck[4] = grepl(pattern = "Inactivity - You have been logged out", x = page, ignore.case = T)
  
  badReturnCheck = any(badReturnCheck)
  
  return(badReturnCheck)
} #/BadReturnCheck function
