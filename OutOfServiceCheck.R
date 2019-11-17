# OutOfServiceCheck.R
# page = x

# Given a page pulled from the Scantron website, this function determines whether it's saying that it's temporarily out of service
# 

#' @title Out Of Service Check
#' @description Check whether the website is temporarily out of service
#' @param page the content of a page pull from the Scantron website
#' @param messageLevel integer of length 1 indicating level of diagnostic messages to print
#' @return Logical.  If the service is unavailable, returns TRUE.  Otherwise, FALSE.
#' @details This function checks for several different text strings that might appear in and Out Of Service page.
OutOfServiceCheck = function(page, messageLevel = 0){
  
  if(messageLevel > 0){
    print("Checking to see if the site is down.")
  }
  
  OOSCheck = rep(F,4)
  OOSCheck[1] = grepl(pattern = "Temporarily Out Of Service", x = page, ignore.case = T)
  OOSCheck[2] = grepl(pattern = "The application you tried to reach is currently unavailable.", x = page, ignore.case = T)
  OOSCheck[3] = grepl(pattern = "During this down time we are performing necessary system maintenance", x = page, ignore.case = T)
  OOSCheck[4] = grepl(pattern = "We estimate the service will be unavailable", x = page, ignore.case = T)
  
  OOSCheck = any(OOSCheck)
  
  return(OOSCheck)
} #/OutOfServiceCheck function
